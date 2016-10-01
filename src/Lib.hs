{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



{- Exports -}
module Lib
    ( parseRomeOptions
    , runRomeWithOptions
    , discoverRegion
    ) where



{- Imports -}
import qualified Codec.Archive.Zip            as Zip
import           Control.Applicative          ((<|>))
import           Control.Lens                 hiding (List)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.Trans          (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy         as L
import           Data.Cartfile
import           Data.Romefile
import           Data.Char                    (isSpace)
import           Data.Conduit.Binary          (sinkLbs)
import           Data.Ini                     as INI
import           Data.Ini.Utils               as INI
import           Data.String.Utils
import qualified Data.Map                     as M
import           Data.Maybe
import qualified Data.Text                    as T
import qualified Network.AWS                  as AWS
import           Network.AWS.Data
import           Network.AWS.S3               as S3
import           Options.Applicative          as Opts
import           System.Directory
import           System.Environment



{- Types -}

type Config        = (AWS.Env, Bool)
type RomeMonad     = ExceptT String IO

data RomeCommand = Upload [FrameworkName]
                  | Download [FrameworkName]
                  | List ListMode
                  deriving (Show, Eq)

data ListMode = All
               | Missing
               | Present
               deriving (Show, Eq)

data RomeOptions = RomeOptions { romeCommand :: RomeCommand
                               , verbose     :: Bool
                               }



{- Functions -}
uploadParser :: Opts.Parser RomeCommand
uploadParser = pure Upload <*> Opts.many (Opts.argument str (Opts.metavar "FRAMEWORKS..." <> Opts.help "Zero or more framework names. If zero, all frameworks and dSYMs are uploaded."))

downloadParser :: Opts.Parser RomeCommand
downloadParser = pure Download <*> Opts.many (Opts.argument str (Opts.metavar "FRAMEWORKS..." <> Opts.help "Zero or more framework names. If zero, all frameworks and dSYMs are downloaded."))

listParser :: Opts.Parser RomeCommand
listParser = pure List <*> (
                            (Opts.flag' Missing (Opts.long "missing" <> Opts.help "List frameworks missing from the cache. Ignores dSYMs")
                            <|> Opts.flag' Present (Opts.long "present" <> Opts.help "List frameworks present in the cache. Ignores dSYMs.")
                           )
                           <|> Opts.flag All All (Opts.help "Reports missing or present status of frameworks in the cache. Ignores dSYMs."))

parseRomeCommand :: Opts.Parser RomeCommand
parseRomeCommand = Opts.subparser $
  Opts.command "upload" (uploadParser `withInfo` "Uploads frameworks and dSYMs contained in the local Carthage/Build/iOS to S3, according to the local Cartfile.resolved")
  <> Opts.command "download" (downloadParser `withInfo` "Downloads and unpacks in Carthage/Build/iOS frameworks and dSYMs found in S3, according to the local Carftfile.resolved")
  <> Opts.command "list" (listParser `withInfo` "Lists frameworks in the cache and reports cache misses/hits, according to the local Carftfile.resolved. Ignores dSYMs.")

parseRomeOptions :: Opts.Parser RomeOptions
parseRomeOptions = RomeOptions <$> parseRomeCommand <*> Opts.switch ( Opts.short 'v' <> help "Show verbose output" )

withInfo :: Opts.Parser a -> String -> Opts.ParserInfo a
withInfo opts desc = Opts.info (Opts.helper <*> opts) $ Opts.progDesc desc

getCartfileEntires :: RomeMonad [CartfileEntry]
getCartfileEntires = do
  eitherCartfileEntries <- liftIO $ parseCartfileResolved cartfileResolved
  case eitherCartfileEntries of
    Left e -> throwError $ "Carfile.resolved parse error: " ++ show e
    Right cartfileEntries -> return cartfileEntries

getRomefileEntries :: RomeMonad (S3.BucketName, [RomefileEntry])
getRomefileEntries = do
  (bucketName, entries) <- parseRomefile romefile
  return (S3.BucketName bucketName, entries)

runRomeWithOptions :: AWS.Env -> RomeOptions -> ExceptT String IO ()
runRomeWithOptions env (RomeOptions options verbose) = do
  cartfileEntries <- getCartfileEntires
  (s3BucketName, romefileEntries) <- getRomefileEntries
  case options of
      Upload [] -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom cartfileEntries romefileEntries
        liftIO $ runReaderT (uploadFrameworksAndDsymsToS3 s3BucketName frameworkAndVersions) (env, verbose)

      Upload names ->
        liftIO $ runReaderT (uploadFrameworksAndDsymsToS3 s3BucketName (filterByNames cartfileEntries romefileEntries names)) (env, verbose)

      Download [] -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom cartfileEntries romefileEntries
        liftIO $ runReaderT (downloadFrameworksAndDsymsFromS3 s3BucketName frameworkAndVersions) (env, verbose)

      Download names ->
        liftIO $ runReaderT (downloadFrameworksAndDsymsFromS3 s3BucketName (filterByNames cartfileEntries romefileEntries names)) (env, verbose)

      List listMode -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom cartfileEntries romefileEntries
        existing <- liftIO $ runReaderT (probeForFrameworks s3BucketName frameworkAndVersions) (env, verbose)
        let t = toInvertedRomeFilesEntriesMap romefileEntries
        let namesVersionAndExisting = replaceKnownFrameworkNamesWitGitRepoNamesInProbeResults (toInvertedRomeFilesEntriesMap romefileEntries) . filterAccordingToListMode listMode $  zip frameworkAndVersions existing
        liftIO $ mapM_ (printProbeResult listMode) namesVersionAndExisting

  where
    constructFrameworksAndVersionsFrom cartfileEntries romefileEntries = deriveFrameworkNamesAndVersion (toRomeFilesEntriesMap romefileEntries) cartfileEntries
    filterByNames cartfileEntries romefileEntries = concatMap (constructFrameworksAndVersionsFrom cartfileEntries romefileEntries `filterByName`)

fromErrorMessage :: AWS.ErrorMessage -> String
fromErrorMessage (AWS.ErrorMessage t) = T.unpack t

filterByName:: [(FrameworkName, Version)] -> FrameworkName -> [(FrameworkName, Version)]
filterByName fs s = filter (\(name, version) -> name == s) fs

uploadFrameworksAndDsymsToS3 s3Bucket = mapM_ (uploadFrameworkAndDsymToS3 s3Bucket)

uploadFrameworkAndDsymToS3 s3BucketName fv@(framework, version) = do
  (env, verbose) <- ask
  frameworkExists <- liftIO $ doesDirectoryExist frameworkDirectory
  dSymExists <- liftIO $ doesDirectoryExist dSYMdirectory
  when frameworkExists $ do
    frameworkArchive <- zipDir frameworkDirectory verbose
    uploadBinary s3BucketName (Zip.fromArchive frameworkArchive) (framework ++ "/" ++ frameworkArchiveName fv) framework
  when dSymExists $ do
    dSYMArchive <- zipDir dSYMdirectory verbose
    uploadBinary s3BucketName (Zip.fromArchive dSYMArchive) (framework ++ "/" ++ dSYMArchiveName fv) (framework ++ ".dSYM")
  where
    carthageBuildDirecotryiOS = "Carthage/Build/iOS/"
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo framework
    frameworkDirectory = carthageBuildDirecotryiOS ++ frameworkNameWithFrameworkExtension
    dSYMNameWithDSYMExtension = frameworkNameWithFrameworkExtension ++ ".dSYM"
    dSYMdirectory = carthageBuildDirecotryiOS ++ dSYMNameWithDSYMExtension
    zipDir dir verbose = liftIO $ Zip.addFilesToArchive (zipOptions verbose) Zip.emptyArchive [dir]

uploadBinary s3BucketName binaryZip destinationPath frameworkName = do
  let objectKey = S3.ObjectKey $ T.pack destinationPath
  (env, verbose) <- ask
  runResourceT . AWS.runAWS env $ do
    let body = AWS.toBody binaryZip
    rs <- AWS.trying AWS._Error (AWS.send $ S3.putObject s3BucketName objectKey body)
    case rs of
      Left e -> sayLn $ "Error uploading " <> frameworkName <> " : " <> errorString e
      Right _ -> sayLn $ "Successfully uploaded " <> frameworkName <> " to: " <> destinationPath

downloadFrameworksAndDsymsFromS3 s3BucketName = mapM_ (downloadFrameworkAndDsymFromS3 s3BucketName)

downloadFrameworkAndDsymFromS3 s3BucketName fv@(frameworkName, version) = do
  let frameworkZipName = frameworkArchiveName fv
  let dSYMZipName = dSYMArchiveName fv
  let frameworkObjectKey = S3.ObjectKey . T.pack $ frameworkName ++ "/" ++ frameworkZipName
  let dSYMObjectKey = S3.ObjectKey . T.pack $ frameworkName ++ "/" ++ dSYMZipName
  (env, verbose) <- ask
  runResourceT . AWS.runAWS env $ getZip s3BucketName frameworkObjectKey frameworkZipName verbose
  runResourceT . AWS.runAWS env $ getZip s3BucketName dSYMObjectKey dSYMZipName verbose

getZip s3BucketName frameworkObjectKey zipName verbose = do
  rs <- AWS.trying AWS._Error (AWS.send $ S3.getObject s3BucketName frameworkObjectKey)
  case rs of
    Left e -> sayLn $ "Error downloading " <> zipName <> " : " <> errorString e
    Right goResponse -> do
      lbs <- lift $ view S3.gorsBody goResponse `AWS.sinkBody` sinkLbs
      sayLn $ "Downloaded: " ++ zipName
      liftIO $ Zip.extractFilesFromArchive (zipOptions verbose) (Zip.toArchive lbs)
      sayLn $ "Unzipped: " ++ zipName


probeForFrameworks s3BucketName = mapM (probeForFramework s3BucketName)

probeForFramework s3BucketName (frameworkName, version) = do
  let frameworkZipName = frameworkArchiveName (frameworkName, version)
  let frameworkObjectKey = S3.ObjectKey . T.pack $ frameworkName ++ "/" ++ frameworkZipName
  (env, verbose) <- ask
  runResourceT . AWS.runAWS env $ checkIfFrameworkExistsInBucket s3BucketName frameworkObjectKey verbose


checkIfFrameworkExistsInBucket s3BucketName frameworkObjectKey verbose = do
  rs <- AWS.trying AWS._Error (AWS.send $ S3.headObject s3BucketName frameworkObjectKey)
  case rs of
    Left e -> return False
    Right hoResponse -> return True

errorString :: AWS.Error -> String
errorString e = fromErrorMessage $ fromMaybe (AWS.ErrorMessage "Unexpected Error") maybeServiceError
  where
    maybeServiceError = view AWS.serviceMessage =<< (e ^? AWS._ServiceError)

sayLn :: MonadIO m => String -> m ()
sayLn = liftIO . putStrLn

zipOptions :: Bool -> [Zip.ZipOption]
zipOptions verbose = if verbose then [Zip.OptRecursive, Zip.OptVerbose] else [Zip.OptRecursive]

deriveFrameworkNamesAndVersion :: M.Map GitRepoName [FrameworkName] -> [CartfileEntry] -> [(FrameworkName, Version)]
deriveFrameworkNamesAndVersion romeMap = concatMap (deriveFrameworkNameAndVersion romeMap)

deriveFrameworkNameAndVersion ::  M.Map GitRepoName [FrameworkName] -> CartfileEntry -> [(FrameworkName, Version)]
deriveFrameworkNameAndVersion romeMap (CartfileEntry GitHub l v) = map (\n -> (n, v)) $ fromMaybe [gitHubRepositoryName] (M.lookup (GitRepoName gitHubRepositoryName) romeMap)
  where
    gitHubRepositoryName = last $ splitWithSeparator '/' l
deriveFrameworkNameAndVersion romeMap (CartfileEntry Git l v)    = map (\n -> (n, v)) $ fromMaybe [gitRepositoryName] (M.lookup (GitRepoName gitRepositoryName) romeMap)
  where
    gitRepositoryName = getGitRepositoryNameFromGitURL l
    getGitRepositoryNameFromGitURL = replace ".git" "" . last . splitWithSeparator '/'

appendFrameworkExtensionTo :: FrameworkName -> String
appendFrameworkExtensionTo a = a ++ ".framework"

frameworkArchiveName :: (String, Version) -> String
frameworkArchiveName (name, version) = appendFrameworkExtensionTo name ++ "-" ++ version ++ ".zip"

dSYMArchiveName :: (String, Version) -> String
dSYMArchiveName (name, version) = appendFrameworkExtensionTo name ++ ".dSYM" ++ "-" ++ version ++ ".zip"

splitWithSeparator :: (Eq a) => a -> [a] -> [[a]]
splitWithSeparator _ [] = []
splitWithSeparator a as = g as : splitWithSeparator a (dropTaken as as)
    where
      numberOfAsIn = length . takeWhile (== a)
      g = takeWhile (/= a) . dropWhile (== a)
      dropTaken bs = drop $ numberOfAsIn bs + length (g bs)

printProbeResult :: MonadIO m => ListMode -> ((String, Version), Bool) -> m ()
printProbeResult listMode ((frameworkName, version), present) | listMode == Missing || listMode ==  Present = sayLn frameworkName
                                                              | otherwise                                   = sayLn $ frameworkName <> " " <> version <> " " <> printProbeStringForBool present

printProbeStringForBool :: Bool -> String
printProbeStringForBool True  = green <> "✔︎" <> noColor
printProbeStringForBool False = red <> "✘" <> noColor

red :: String
red = "\ESC[0;31m"

green :: String
green = "\ESC[0;32m"

noColor :: String
noColor = "\ESC[0m"

filterAccordingToListMode :: ListMode -> [((String, Version), Bool)] -> [((String, Version), Bool)]
filterAccordingToListMode All probeResults     = probeResults
filterAccordingToListMode Missing probeResults = (\((name, version), present) -> not present) `filter`probeResults
filterAccordingToListMode Present probeResults = (\((name, version), present) -> present) `filter`probeResults

replaceKnownFrameworkNamesWitGitRepoNamesInProbeResults :: M.Map FrameworkName GitRepoName -> [((FrameworkName, Version), Bool)] -> [((String, Version), Bool)]
replaceKnownFrameworkNamesWitGitRepoNamesInProbeResults reverseRomeMap = map (replaceResultIfFrameworkNameIsInMap (reverseRomeMap))
  where
    replaceResultIfFrameworkNameIsInMap :: M.Map FrameworkName GitRepoName -> ((FrameworkName, Version), Bool) -> ((String, Version), Bool)
    -- replaceResultIfFrameworkNameIsInMap reverseRomeMap ((frameworkName, version), present) = ((fromMaybe (frameworkName) (fmap unGitRepoName (M.lookup reverseRomeMap)), version), present)
    replaceResultIfFrameworkNameIsInMap reverseRomeMap ((frameworkName, version), present) = ((fromMaybe frameworkName (fmap unGitRepoName (M.lookup frameworkName reverseRomeMap)), version), present)


s3ConfigFile :: (MonadIO m) => m FilePath
s3ConfigFile = (++ p) `liftM` liftIO getHomeDirectory
    where
      p = "/.aws/config"

discoverRegion :: RomeMonad AWS.Region
discoverRegion = do
  f <- s3ConfigFile
  profile <- liftIO $ lookupEnv "AWS_PROFILE"
  getRegionFromFile f (fromMaybe "default" profile)

getRegionFromFile :: FilePath -> String -> RomeMonad AWS.Region
getRegionFromFile f profile = do
  i <- liftIO (INI.readIniFile f)
  case i of
    Left e -> throwError e
    Right ini -> do
      region <- withExceptT (\e -> "Could not parse " <> f <> ": " <> T.unpack e) $ INI.requireKey "region" `INI.inRequiredSection` T.pack profile `INI.fromIni''` ini
      let eitherAWSRegion = fromText region :: Either String AWS.Region
      case eitherAWSRegion of
        Left e  -> throwError e
        Right r -> return r


toRomeFilesEntriesMap :: [RomefileEntry] -> M.Map GitRepoName [FrameworkName]
toRomeFilesEntriesMap = M.fromList . map romeFileEntryToTuple

toInvertedRomeFilesEntriesMap :: [RomefileEntry] -> M.Map FrameworkName GitRepoName
toInvertedRomeFilesEntriesMap = M.fromList . concatMap romeFileEntryToListOfTuples
  where listify (fs, g) = map (\f -> (f,g)) fs
        flipTuple = uncurry (flip (,))
        romeFileEntryToListOfTuples = listify . flipTuple . romeFileEntryToTuple

romeFileEntryToTuple :: RomefileEntry -> (GitRepoName, [FrameworkName])
romeFileEntryToTuple RomefileEntry {..} = (gitRepositoryName, frameworkCommonNames)

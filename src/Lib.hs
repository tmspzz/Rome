{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



{- Exports -}
module Lib
    ( parseRomeOptions
    , runRomeWithOptions
    , discoverRegion
    , filterByNameEqualTo
    , filterOutFrameworkNamesAndVersionsIfNotIn
    , splitWithSeparator
    ) where



{- Imports -}
import qualified Codec.Archive.Zip            as Zip
import           Control.Applicative          ((<|>))
import           Control.Lens                 hiding (List)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.Trans          (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy         as L
import           Data.Cartfile
import           Data.Char                    (isSpace)
import           Data.Conduit                 (($$))
import           Data.Conduit.Binary          (sinkFile, sourceFile , sinkLbs, sourceLbs)
import           Data.Ini                     as INI
import           Data.Ini.Utils               as INI
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Romefile
import           Data.String.Utils
import qualified Data.Text                    as T
import           Data.Time
import qualified Network.AWS                  as AWS
import           Network.AWS.Data
import           Network.AWS.S3               as S3
import           Options.Applicative          as Opts
import           System.Directory
import           System.Environment
import           System.FilePath



{- Types -}

type Config                = (AWS.Env, Bool)
type RomeMonad             = ExceptT String IO
type RepositoryMap         = M.Map GitRepoName [FrameworkName]
type InvertedRepositoryMap = M.Map FrameworkName GitRepoName

data RomeCommand = Upload [GitRepoName]
                  | Download [GitRepoName]
                  | List ListMode
                  deriving (Show, Eq)

data ListMode = All
               | Missing
               | Present
               deriving (Show, Eq)

data RomeOptions = RomeOptions { romeCommand :: RomeCommand
                               , verbose     :: Bool
                               }


{- Constants -}
carthageBuildDirecotryiOS :: String
carthageBuildDirecotryiOS = "Carthage/Build/iOS/"


{- Functions -}
uploadParser :: Opts.Parser RomeCommand
uploadParser = pure Upload <*> Opts.many (Opts.argument (GitRepoName <$> str) (Opts.metavar "FRAMEWORKS..." <> Opts.help "Zero or more framework names. If zero, all frameworks and dSYMs are uploaded."))

downloadParser :: Opts.Parser RomeCommand
downloadParser = pure Download <*> Opts.many (Opts.argument (GitRepoName <$> str) (Opts.metavar "FRAMEWORKS..." <> Opts.help "Zero or more framework names. If zero, all frameworks and dSYMs are downloaded."))

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

getRomefileEntries :: RomeMonad RomeFileParseResult
getRomefileEntries = parseRomefile romefile

runRomeWithOptions :: AWS.Env -> RomeOptions -> RomeMonad ()
runRomeWithOptions env (RomeOptions options verbose) = do
  cartfileEntries <- getCartfileEntires
  RomeFileParseResult { .. } <- getRomefileEntries
  let respositoryMap = toRomeFilesEntriesMap repositoryMapEntries
  let ignoreNames = concatMap frameworkCommonNames ignoreMapEntries
  case options of

      Upload [] -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom cartfileEntries respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        liftIO $ runReaderT (uploadFrameworksAndDsymsToCaches cacheInfo frameworkAndVersions) (env, verbose)

      Upload gitRepoNames -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom  (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        liftIO $ runReaderT (uploadFrameworksAndDsymsToCaches cacheInfo frameworkAndVersions) (env, verbose)

      Download [] -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom cartfileEntries respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        liftIO $ runReaderT (downloadFrameworksAndDsymsFromCaches cacheInfo frameworkAndVersions) (env, verbose)

      Download gitRepoNames -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom  (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        liftIO $ runReaderT (downloadFrameworksAndDsymsFromCaches cacheInfo frameworkAndVersions) (env, verbose)

      List listMode -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom cartfileEntries respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        existing <- liftIO $ runReaderT (probeCachesForFrameworks cacheInfo frameworkAndVersions) (env, verbose)
        let namesVersionAndExisting = replaceKnownFrameworkNamesWitGitRepoNamesInProbeResults (toInvertedRomeFilesEntriesMap repositoryMapEntries) . filterAccordingToListMode listMode $  zip frameworkAndVersions existing
        liftIO $ mapM_ (printProbeResult listMode) namesVersionAndExisting

  where

    constructFrameworksAndVersionsFrom :: [CartfileEntry] -> RepositoryMap -> [(FrameworkName, Version)]
    constructFrameworksAndVersionsFrom cartfileEntries repositoryMap = deriveFrameworkNamesAndVersion repositoryMap cartfileEntries
    filterRepoMapByGitRepoNames :: RepositoryMap -> [GitRepoName] -> RepositoryMap
    filterRepoMapByGitRepoNames repoMap gitRepoNames = M.unions $ map (restrictRepositoryMapToGitRepoName repoMap) gitRepoNames

fromErrorMessage :: AWS.ErrorMessage -> String
fromErrorMessage (AWS.ErrorMessage t) = T.unpack t

filterByNameEqualTo :: [(FrameworkName, Version)] -> FrameworkName -> [(FrameworkName, Version)]
filterByNameEqualTo fs s = filter (\(name, version) -> name == s) fs

filterOutFrameworkNamesAndVersionsIfNotIn ::[(FrameworkName, Version)] -> [FrameworkName] -> [(FrameworkName, Version)]
filterOutFrameworkNamesAndVersionsIfNotIn favs fns = [fv |  fv <- favs,  fst fv `notElem` fns]

restrictRepositoryMapToGitRepoName:: RepositoryMap -> GitRepoName -> RepositoryMap
restrictRepositoryMapToGitRepoName repoMap repoName = maybe M.empty (M.singleton repoName) $ repoName `M.lookup` repoMap

uploadFrameworksAndDsymsToCaches :: RomeCacheInfo -> [(FrameworkName, Version)] -> ReaderT (AWS.Env, Bool) IO ()
uploadFrameworksAndDsymsToCaches cacheInfo = mapM_ (uploadFrameworkAndDsymToCaches cacheInfo)

uploadFrameworkAndDsymToCaches :: RomeCacheInfo -> (FrameworkName, Version) -> ReaderT (AWS.Env, Bool) IO ()
uploadFrameworkAndDsymToCaches  (RomeCacheInfo bucketName localCacheDir) fv@(f@(FrameworkName fwn), version) = do
  readerEnv@(env, verbose) <- ask
  frameworkExists <- liftIO $ doesDirectoryExist frameworkDirectory
  dSymExists <- liftIO $ doesDirectoryExist dSYMdirectory
  when frameworkExists $ do
    when verbose $
      sayLnWithTime $ "Staring to zip: " <> frameworkDirectory
    frameworkArchive <- zipDir frameworkDirectory verbose
    runMaybeT $
      MaybeT (return localCacheDir)
        >>= \dir -> liftIO $
                      runReaderT (saveBinaryToLocalCache dir (Zip.fromArchive frameworkArchive) remoteFrameworkUploadPath fwn) readerEnv
    uploadBinary s3BucketName (Zip.fromArchive frameworkArchive) remoteFrameworkUploadPath fwn
  when dSymExists $ do
    when verbose $
      sayLnWithTime $ "Staring to zip: " <> dSYMdirectory
    dSYMArchive <- zipDir dSYMdirectory verbose
    runMaybeT $
      MaybeT (return localCacheDir)
        >>= \dir -> liftIO $
                      runReaderT (saveBinaryToLocalCache dir (Zip.fromArchive dSYMArchive) remoteDsymUploadPath dSYMNameWithDSYMExtension) readerEnv
    uploadBinary s3BucketName (Zip.fromArchive dSYMArchive) remoteDsymUploadPath (fwn ++ ".dSYM")

  where

    s3BucketName = S3.BucketName bucketName
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    frameworkDirectory = carthageBuildDirecotryiOS ++ frameworkNameWithFrameworkExtension
    remoteFrameworkUploadPath = remoteFrameworkPath f version
    dSYMNameWithDSYMExtension = frameworkNameWithFrameworkExtension ++ ".dSYM"
    dSYMdirectory = carthageBuildDirecotryiOS ++ dSYMNameWithDSYMExtension
    remoteDsymUploadPath = fwn ++ "/" ++ dSYMArchiveName fv
    zipDir dir verbose = liftIO $ Zip.addFilesToArchive (zipOptions verbose) Zip.emptyArchive [dir]

uploadBinary s3BucketName binaryZip destinationPath objectName = do
  let objectKey = S3.ObjectKey $ T.pack destinationPath
  (env, verbose) <- ask
  runResourceT . AWS.runAWS env $ do
    let body = AWS.toBody binaryZip
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose $
      sayFunc $ "Started uploading " <> objectName <> " to: " <> destinationPath
    rs <- AWS.trying AWS._Error (AWS.send $ S3.putObject s3BucketName objectKey body)
    case rs of
      Left e -> sayFunc $ "Error uploading " <> objectName <> ": " <> errorString e
      Right _ -> sayFunc $ "Uploaded " <> objectName <> " to: " <> destinationPath

saveBinaryToLocalCache :: FilePath -> L.ByteString -> FilePath -> String -> ReaderT (AWS.Env, Bool) IO ()
saveBinaryToLocalCache cachePath binaryZip destinationPath objectName = do
  (_, verbose) <- ask
  when verbose $
    sayLnWithTime $ "Copying " <> objectName <> " to: " <> finalPath
  liftIO $ createDirectoryIfMissing True (dropFileName finalPath)
  liftIO . runResourceT $ sourceLbs binaryZip $$ sinkFile finalPath
  where
    finalPath = cachePath </> destinationPath

downloadFrameworksAndDsymsFromCaches :: RomeCacheInfo -> [(FrameworkName, Version)] -> ReaderT (AWS.Env, Bool) IO ()
downloadFrameworksAndDsymsFromCaches cacheInfo = mapM_ (downloadFrameworkAndDsymFromCaches cacheInfo)

downloadFrameworkAndDsymFromCaches (RomeCacheInfo bucketName localCacheDir) fv@(f@(FrameworkName fwn), version) = do
  readerEnv@(env, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn

  case localCacheDir of
    Just cacheDir -> do
      frameworkExistsInLocalCache <- liftIO . doesFileExist $ cacheDir </> remoteFrameworkUploadPath
      when frameworkExistsInLocalCache $ do
        binary <- runResourceT $ sourceFile (cacheDir </> remoteFrameworkUploadPath) $$ sinkLbs
        unzipBinary binary fwn frameworkZipName
      unless frameworkExistsInLocalCache $ do
        eitherFrameworkBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remoteFrameworkUploadPath fwn
        case eitherFrameworkBinary of
          Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> errorString e
          Right frameworkBinary -> saveBinaryToLocalCache cacheDir frameworkBinary remoteFrameworkUploadPath fwn

      dSYMExistsInLocalCache <- liftIO . doesFileExist $ cacheDir </> remotedSYMUploadPath
      when dSYMExistsInLocalCache $ do
        binary <- runResourceT $ sourceFile (cacheDir </> remotedSYMUploadPath) $$ sinkLbs
        unzipBinary binary fwn dSYMZipName
      unless dSYMExistsInLocalCache $ do
        eitherdSYMBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotedSYMUploadPath (fwn ++ ".dSYM")
        case eitherdSYMBinary of
          Left e -> sayFunc $ "Error downloading " <> (fwn ++ ".dSYM") <> " : " <> errorString e
          Right dSYMBinary -> saveBinaryToLocalCache cacheDir dSYMBinary remotedSYMUploadPath (fwn ++ ".dSYM")

    Nothing -> do
      eitherFrameworkBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remoteFrameworkUploadPath fwn
      case eitherFrameworkBinary of
        Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> errorString e
        Right frameworkBinary -> unzipBinary frameworkBinary fwn frameworkZipName
      eitherdSYMBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotedSYMUploadPath (fwn ++ ".dSYM")
      case eitherdSYMBinary of
        Left e -> sayFunc $ "Error downloading " <> (fwn ++ ".dSYM") <> " : " <> errorString e
        Right dSYMBinary -> unzipBinary dSYMBinary fwn dSYMZipName

  where
    s3BucketName = S3.BucketName bucketName
    frameworkZipName = frameworkArchiveName f version
    remoteFrameworkUploadPath = remoteFrameworkPath f version
    dSYMZipName = dSYMArchiveName fv
    remotedSYMUploadPath = fwn ++ "/" ++ dSYMZipName

downloadBinary s3BucketName objectRemotePath objectName = do
  readerEnv@(env, verbose) <- ask
  runResourceT . AWS.runAWS env $ do
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose $
      sayFunc $ "Started downloading " <> objectName <> " from: " <> objectRemotePath
    rs <- AWS.send $ S3.getObject s3BucketName objectKey
    binary <- view S3.gorsBody rs `AWS.sinkBody` sinkLbs
    sayFunc $ "Downloaded " <> objectName <> " from: " <> objectRemotePath
    return binary

  where
    objectKey = S3.ObjectKey . T.pack $ objectRemotePath

unzipBinary :: MonadIO m => L.ByteString -> String -> String -> ReaderT (AWS.Env, Bool) m ()
unzipBinary objectBinary objectName objectZipName = do
  (_, verbose) <- ask
  when verbose $
   sayLnWithTime $ "Staring to unzip " <> objectZipName
  liftIO $ Zip.extractFilesFromArchive (zipOptions verbose) (Zip.toArchive objectBinary)
  when verbose $
    sayLnWithTime $ "Unzipped " <> objectName <> " from: " <> objectZipName



remoteFrameworkPath :: FrameworkName -> Version -> String
remoteFrameworkPath f@(FrameworkName fwn) v = fwn ++ "/" ++ frameworkArchiveName f v

probeCachesForFrameworks :: RomeCacheInfo -> [(FrameworkName, Version)] -> ReaderT (AWS.Env, Bool) IO [Bool]
probeCachesForFrameworks cacheInfo = mapM (probeCachesForFramework cacheInfo)

probeCachesForFramework :: RomeCacheInfo -> (FrameworkName, Version) -> ReaderT (AWS.Env, Bool) IO Bool
probeCachesForFramework (RomeCacheInfo bucketName localCacheDir) (f@(FrameworkName fwn), v) = do
  (env, verbose) <- ask
  runResourceT . AWS.runAWS env $ checkIfFrameworkExistsInBucket s3BucketName frameworkObjectKey verbose
  where
    s3BucketName = S3.BucketName bucketName
    frameworkZipName = frameworkArchiveName f v
    frameworkObjectKey = S3.ObjectKey . T.pack $ fwn ++ "/" ++ frameworkZipName

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

sayLnWithTime :: MonadIO m => String -> m ()
sayLnWithTime line = do
  time <- liftIO getZonedTime
  sayLn $ formatTime defaultTimeLocale "%T %F" time <> " - " <> line

zipOptions :: Bool -> [Zip.ZipOption]
zipOptions verbose = if verbose then [Zip.OptRecursive, Zip.OptVerbose] else [Zip.OptRecursive]

deriveFrameworkNamesAndVersion :: RepositoryMap -> [CartfileEntry] -> [(FrameworkName, Version)]
deriveFrameworkNamesAndVersion romeMap = concatMap (deriveFrameworkNameAndVersion romeMap)

deriveFrameworkNameAndVersion ::  RepositoryMap -> CartfileEntry -> [(FrameworkName, Version)]
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry GitHub (Location l) v) = map (\n -> (n, v)) $ fromMaybe [FrameworkName gitHubRepositoryName] (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
  where
    gitHubRepositoryName = unGitRepoName $ gitRepoNameFromCartfileEntry cfe
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry Git (Location l) v)    = map (\n -> (n, v)) $ fromMaybe [FrameworkName gitRepositoryName] (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
  where
    gitRepositoryName = unGitRepoName $ gitRepoNameFromCartfileEntry cfe

gitRepoNameFromCartfileEntry :: CartfileEntry -> GitRepoName
gitRepoNameFromCartfileEntry (CartfileEntry GitHub (Location l) _) = GitRepoName . last . splitWithSeparator '/' $ l
gitRepoNameFromCartfileEntry (CartfileEntry Git (Location l) _) = GitRepoName . replace ".git" "" . last . splitWithSeparator '/' $ l

filterCartfileEntriesByGitRepoNames :: [GitRepoName] -> [CartfileEntry] -> [CartfileEntry]
filterCartfileEntriesByGitRepoNames repoNames cartfileEntries = [c | c <- cartfileEntries, gitRepoNameFromCartfileEntry c `elem` repoNames]

appendFrameworkExtensionTo :: FrameworkName -> String
appendFrameworkExtensionTo (FrameworkName a) = a ++ ".framework"

frameworkArchiveName :: FrameworkName -> Version -> String
frameworkArchiveName f (Version v)  = appendFrameworkExtensionTo f ++ "-" ++ v ++ ".zip"

dSYMArchiveName :: (FrameworkName, Version) -> String
dSYMArchiveName (fwn, Version v) = appendFrameworkExtensionTo fwn ++ ".dSYM" ++ "-" ++ v ++ ".zip"

splitWithSeparator :: Char -> String -> [String]
splitWithSeparator a as = map T.unpack (T.split (== a) $ T.pack as)

printProbeResult :: MonadIO m => ListMode -> ((String, Version), Bool) -> m ()
printProbeResult listMode ((frameworkName, Version v), present) | listMode == Missing || listMode ==  Present = sayLn frameworkName
                                                                | otherwise                                   = sayLn $ frameworkName <> " " <> v <> " " <> printProbeStringForBool present

printProbeStringForBool :: Bool -> String
printProbeStringForBool True  = green <> "✔︎" <> noColor
printProbeStringForBool False = red <> "✘" <> noColor

red :: String
red = "\ESC[0;31m"

green :: String
green = "\ESC[0;32m"

noColor :: String
noColor = "\ESC[0m"

filterAccordingToListMode :: ListMode -> [((FrameworkName, Version), Bool)] -> [((FrameworkName, Version), Bool)]
filterAccordingToListMode All probeResults     = probeResults
filterAccordingToListMode Missing probeResults = (\((FrameworkName fwn, version), present) -> not present) `filter` probeResults
filterAccordingToListMode Present probeResults = (\((FrameworkName fwn, version), present) -> present) `filter` probeResults

replaceKnownFrameworkNamesWitGitRepoNamesInProbeResults :: InvertedRepositoryMap -> [((FrameworkName, Version), Bool)] -> [((String, Version), Bool)]
replaceKnownFrameworkNamesWitGitRepoNamesInProbeResults reverseRomeMap = map (replaceResultIfFrameworkNameIsInMap reverseRomeMap)
  where
    replaceResultIfFrameworkNameIsInMap :: InvertedRepositoryMap -> ((FrameworkName, Version), Bool) -> ((String, Version), Bool)
    replaceResultIfFrameworkNameIsInMap reverseRomeMap ((frameworkName@(FrameworkName fwn), version), present) = ((maybe fwn unGitRepoName (M.lookup frameworkName reverseRomeMap), version), present)

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

toRomeFilesEntriesMap :: [RomefileEntry] -> RepositoryMap
toRomeFilesEntriesMap = M.fromList . map romeFileEntryToTuple

toInvertedRomeFilesEntriesMap :: [RomefileEntry] -> InvertedRepositoryMap
toInvertedRomeFilesEntriesMap = M.fromList . concatMap romeFileEntryToListOfTuples
  where listify (fs, g) = map (\f -> (f,g)) fs
        flipTuple = uncurry (flip (,))
        romeFileEntryToListOfTuples = listify . flipTuple . romeFileEntryToTuple

romeFileEntryToTuple :: RomefileEntry -> (GitRepoName, [FrameworkName])
romeFileEntryToTuple RomefileEntry {..} = (gitRepositoryName, frameworkCommonNames)

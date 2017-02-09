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
import           Control.Monad.Reader         (MonadReader, ReaderT, ask,
                                               runReaderT)
import           Control.Monad.Trans          (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Cartfile
import           Data.Char                    (isLetter)
import qualified Data.Conduit                 as C (Conduit, Sink, await, yield, ($$),
                                                    (=$=))
import qualified Data.Conduit.Binary          as C (sinkFile, sinkLbs,
                                                    sourceFile, sourceLbs)
import           Data.Either.Utils
import           Data.Function
import           Data.GitRepoAvailability
import           Data.Ini                     as INI
import           Data.Ini.Utils               as INI
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Romefile
import           Data.String.Utils
import           Data.TargetPlatform
import qualified Data.Text                    as T
import           Data.Time
import qualified Network.AWS                  as AWS
import           Network.AWS.Data
import           Network.AWS.S3               as S3
import           Options.Applicative          as Opts
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Read

{- Types -}

type UDCEnv                = (AWS.Env{-, VerifyFlag-}, SkipLocalCacheFlag, Bool)
type RomeMonad             = ExceptT String IO
type RepositoryMap         = M.Map GitRepoName [FrameworkName]
type InvertedRepositoryMap = M.Map FrameworkName GitRepoName

data RomeCommand = Upload RomeUDCPayload
                  | Download RomeUDCPayload
                  | List RomeListPayload
                  deriving (Show, Eq)

data RomeUDCPayload = RomeUDCPayload { _payload            :: [GitRepoName]
                                     , _udcPlatforms       :: [TargetPlatform]
                                    --  , _verifyFlag         :: VerifyFlag
                                     , _skipLocalCacheFlag :: SkipLocalCacheFlag
                                     }
                                     deriving (Show, Eq)

-- newtype VerifyFlag = VerifyFlag { _verify :: Bool } deriving (Show, Eq)
newtype SkipLocalCacheFlag = SkipLocalCacheFlag { _skipLocalCache :: Bool } deriving (Show, Eq)

data RomeListPayload = RomeListPayload { _listMode      :: ListMode
                                       , _listPlatforms :: [TargetPlatform]
                                       }
                                       deriving (Show, Eq)

data ListMode = All
               | Missing
               | Present
               deriving (Show, Eq)

data RomeOptions = RomeOptions { romeCommand :: RomeCommand
                               , verbose     :: Bool
                               }


{- Constants -}
carthageBuildDirectory :: TargetPlatform -> FilePath
carthageBuildDirectory platform = "Carthage" </> "Build" </> show platform

{- Commnad line arguments parsing -}

-- verifyParser :: Parser VerifyFlag
-- verifyParser = VerifyFlag <$> Opts.switch ( Opts.long "verify" <> Opts.help "Verify that the framework has the same hash as specified in the Cartfile.resolved.")

skipLocalCacheParser :: Parser SkipLocalCacheFlag
skipLocalCacheParser = SkipLocalCacheFlag <$> Opts.switch ( Opts.long "skip-local-cache" <> Opts.help "Ignore the local cache when performing the operation.")

reposParser :: Opts.Parser [GitRepoName]
reposParser = Opts.many (Opts.argument (GitRepoName <$> str) (Opts.metavar "FRAMEWORKS..." <> Opts.help "Zero or more framework names. If zero, all frameworks and dSYMs are uploaded."))

platformsParser :: Opts.Parser [TargetPlatform]
platformsParser = (nub . concat <$> Opts.some (Opts.option (eitherReader platformListOrError) (Opts.metavar "PLATFORMS" <> Opts.long "platform" <> Opts.help "Applicable platforms for the command. One of iOS, MacOS, tvOS, watchOS, or a comma-separated list of any of these values.")))
  <|> pure allTargetPlatforms
  where
    platformOrError str = maybeToEither ("Unrecognized platform '" ++ str ++ "'") (readMaybe str)
    splitPlatforms str = filter (not . null) $ filter isLetter <$> wordsBy (not . isLetter) str
    platformListOrError str = mapM platformOrError $ splitPlatforms str

udcPayloadParser :: Opts.Parser RomeUDCPayload
udcPayloadParser = RomeUDCPayload <$> reposParser <*> platformsParser {- <*> verifyParser-} <*> skipLocalCacheParser

uploadParser :: Opts.Parser RomeCommand
uploadParser = pure Upload <*> udcPayloadParser

downloadParser :: Opts.Parser RomeCommand
downloadParser = pure Download <*> udcPayloadParser

listModeParser :: Opts.Parser ListMode
listModeParser = (
                    Opts.flag' Missing (Opts.long "missing" <> Opts.help "List frameworks missing from the cache. Ignores dSYMs")
                    <|> Opts.flag' Present (Opts.long "present" <> Opts.help "List frameworks present in the cache. Ignores dSYMs.")
                 )
                <|> Opts.flag All All (Opts.help "Reports missing or present status of frameworks in the cache. Ignores dSYMs.")

listPayloadParser :: Opts.Parser RomeListPayload
listPayloadParser = RomeListPayload <$> listModeParser <*> platformsParser

listParser :: Opts.Parser RomeCommand
listParser = List <$> listPayloadParser

parseRomeCommand :: Opts.Parser RomeCommand
parseRomeCommand = Opts.subparser $
  Opts.command "upload" (uploadParser `withInfo` "Uploads frameworks and dSYMs contained in the local Carthage/Build/<platform> to S3, according to the local Cartfile.resolved")
  <> Opts.command "download" (downloadParser `withInfo` "Downloads and unpacks in Carthage/Build/<platform> frameworks and dSYMs found in S3, according to the local Carftfile.resolved")
  <> Opts.command "list" (listParser `withInfo` "Lists frameworks in the cache and reports cache misses/hits, according to the local Carftfile.resolved. Ignores dSYMs.")

parseRomeOptions :: Opts.Parser RomeOptions
parseRomeOptions = RomeOptions <$> parseRomeCommand <*> Opts.switch ( Opts.short 'v' <> help "Show verbose output" )

withInfo :: Opts.Parser a -> String -> Opts.ParserInfo a
withInfo opts desc = Opts.info (Opts.helper <*> opts) $ Opts.progDesc desc

{- Functions -}

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
  let reverseRepositoryMap = toInvertedRomeFilesEntriesMap repositoryMapEntries
  let ignoreNames = concatMap frameworkCommonNames ignoreMapEntries
  case options of

      Upload (RomeUDCPayload [] platforms {-shouldVerify-} shouldIgnoreLocalCache) -> do
        let frameworkVersions = constructFrameworksAndVersionsFrom cartfileEntries respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        liftIO $ runReaderT (uploadFrameworksAndDsymsToCaches cacheInfo reverseRepositoryMap frameworkVersions platforms) (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)

      Upload (RomeUDCPayload gitRepoNames platforms {-shouldVerify-} shouldIgnoreLocalCache) -> do
        let frameworkVersions = constructFrameworksAndVersionsFrom  (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        liftIO $ runReaderT (uploadFrameworksAndDsymsToCaches cacheInfo reverseRepositoryMap frameworkVersions platforms) (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)

      Download (RomeUDCPayload [] platforms {-shouldVerify-}  shouldIgnoreLocalCache) -> do
        let frameworkVersions = constructFrameworksAndVersionsFrom cartfileEntries respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        liftIO $ runReaderT (downloadFrameworksAndDsymsFromCaches cacheInfo reverseRepositoryMap frameworkVersions platforms) (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)

      Download (RomeUDCPayload gitRepoNames platforms {-shouldVerify-} shouldIgnoreLocalCache) -> do
        let frameworkVersions = constructFrameworksAndVersionsFrom  (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        liftIO $ runReaderT (downloadFrameworksAndDsymsFromCaches cacheInfo reverseRepositoryMap frameworkVersions platforms) (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)

      List (RomeListPayload listMode platforms) -> do
        let frameworkVersions = constructFrameworksAndVersionsFrom cartfileEntries respositoryMap `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        availabilities <- liftIO $ runReaderT (probeCachesForFrameworks cacheInfo reverseRepositoryMap frameworkVersions platforms) (env, verbose)
        let repoAvailabilities = getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRepositoryMap availabilities
        let repoLines = filter (not . null) $ fmap (formattedRepoAvailability listMode) repoAvailabilities
        mapM_ sayLn repoLines

  where

    constructFrameworksAndVersionsFrom :: [CartfileEntry] -> RepositoryMap -> [FrameworkVersion]
    constructFrameworksAndVersionsFrom cartfileEntries repositoryMap = deriveFrameworkNamesAndVersion repositoryMap cartfileEntries
    filterRepoMapByGitRepoNames :: RepositoryMap -> [GitRepoName] -> RepositoryMap
    filterRepoMapByGitRepoNames repoMap gitRepoNames = M.unions $ map (restrictRepositoryMapToGitRepoName repoMap) gitRepoNames

fromErrorMessage :: AWS.ErrorMessage -> String
fromErrorMessage (AWS.ErrorMessage t) = T.unpack t

filterByNameEqualTo :: [FrameworkVersion] -> FrameworkName -> [FrameworkVersion]
filterByNameEqualTo fs s = filter (\(FrameworkVersion name version) -> name == s) fs

filterOutFrameworkNamesAndVersionsIfNotIn :: [FrameworkVersion] -> [FrameworkName] -> [FrameworkVersion]
filterOutFrameworkNamesAndVersionsIfNotIn favs fns = [fv |  fv <- favs,  _frameworkName fv `notElem` fns]

restrictRepositoryMapToGitRepoName:: RepositoryMap -> GitRepoName -> RepositoryMap
restrictRepositoryMapToGitRepoName repoMap repoName = maybe M.empty (M.singleton repoName) $ repoName `M.lookup` repoMap

uploadFrameworksAndDsymsToCaches :: RomeCacheInfo -> InvertedRepositoryMap -> [FrameworkVersion] -> [TargetPlatform] -> ReaderT UDCEnv IO ()
uploadFrameworksAndDsymsToCaches cacheInfo reverseRomeMap fvs = mapM_ (sequence . uploadFramework)
  where
    uploadFramework = mapM (uploadFrameworkAndDsymToCaches cacheInfo reverseRomeMap) fvs

uploadFrameworkAndDsymToCaches :: RomeCacheInfo -> InvertedRepositoryMap -> FrameworkVersion -> TargetPlatform -> ReaderT UDCEnv IO ()
uploadFrameworkAndDsymToCaches  (RomeCacheInfo bucketName localCacheDir) reverseRomeMap fv@(FrameworkVersion f@(FrameworkName fwn) version) platform = do
  readerEnv@(env {-, shouldVerify-}, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  frameworkExists <- liftIO $ doesDirectoryExist frameworkDirectory
  dSymExists <- liftIO $ doesDirectoryExist dSYMdirectory

  when frameworkExists $ do
    when verbose $
      sayLnWithTime $ "Staring to zip: " <> frameworkDirectory
    frameworkArchive <- zipDir frameworkDirectory verbose
    runMaybeT $
      MaybeT (return localCacheDir)
        >>= \dir -> liftIO $
                      unless skipLocalCache $ saveBinaryToLocalCache dir (Zip.fromArchive frameworkArchive) remoteFrameworkUploadPath fwn verbose
    runReaderT (uploadBinary s3BucketName (Zip.fromArchive frameworkArchive) remoteFrameworkUploadPath fwn) (env, verbose)

  when dSymExists $ do
    when verbose $
      sayLnWithTime $ "Staring to zip: " <> dSYMdirectory
    dSYMArchive <- zipDir dSYMdirectory verbose
    runMaybeT $
      MaybeT (return localCacheDir)
        >>= \dir -> liftIO $
                      unless skipLocalCache $ saveBinaryToLocalCache dir (Zip.fromArchive dSYMArchive) remoteDsymUploadPath dSYMNameWithDSYMExtension verbose
    runReaderT (uploadBinary s3BucketName (Zip.fromArchive dSYMArchive) remoteDsymUploadPath (fwn ++ ".dSYM")) (env, verbose)

  where

    s3BucketName = S3.BucketName bucketName
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectory platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    dSYMNameWithDSYMExtension = frameworkNameWithFrameworkExtension ++ ".dSYM"
    dSYMdirectory = platformBuildDirectory </> dSYMNameWithDSYMExtension
    remoteDsymUploadPath = remoteDsymPath platform reverseRomeMap f version
    zipDir dir verbose = liftIO $ Zip.addFilesToArchive (zipOptions False) Zip.emptyArchive [dir]

uploadBinary s3BucketName binaryZip destinationPath objectName = do
  (env, verbose) <- ask
  let objectKey = S3.ObjectKey $ T.pack destinationPath
  runResourceT . AWS.runAWS env $ do
    let body = AWS.toBody binaryZip
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose $
      sayFunc $ "Started uploading " <> objectName <> " to: " <> destinationPath
    rs <- AWS.trying AWS._Error (AWS.send $ S3.putObject s3BucketName objectKey body)
    case rs of
      Left e -> sayFunc $ "Error uploading " <> objectName <> ": " <> errorString e
      Right _ -> sayFunc $ "Uploaded " <> objectName <> " to: " <> destinationPath

saveBinaryToLocalCache :: MonadIO m => FilePath -> LBS.ByteString -> FilePath -> String -> Bool -> m ()
saveBinaryToLocalCache cachePath binaryZip destinationPath objectName verbose = do
  when verbose $
    sayLnWithTime $ "Copying " <> objectName <> " to: " <> finalPath
  liftIO $ createDirectoryIfMissing True (dropFileName finalPath)
  liftIO . runResourceT $ C.sourceLbs binaryZip C.$$ C.sinkFile finalPath
  where
    finalPath = cachePath </> destinationPath

downloadFrameworksAndDsymsFromCaches :: RomeCacheInfo -> InvertedRepositoryMap -> [FrameworkVersion] -> [TargetPlatform] -> ReaderT UDCEnv IO ()
downloadFrameworksAndDsymsFromCaches cacheInfo reverseRomeMap fvs = mapM_ (sequence . downloadFramework)
  where
    downloadFramework = mapM (downloadFrameworkAndDsymFromCaches cacheInfo reverseRomeMap) fvs

downloadFrameworkAndDsymFromCaches :: RomeCacheInfo -> InvertedRepositoryMap -> FrameworkVersion -> TargetPlatform -> ReaderT UDCEnv IO ()
downloadFrameworkAndDsymFromCaches (RomeCacheInfo bucketName localCacheDir) reverseRomeMap fv@(FrameworkVersion f@(FrameworkName fwn) version) platform = do
  readerEnv@(env{-, shouldVerify-}, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  case localCacheDir of
    Just cacheDir -> do

      let frameworkLocalCachePath = cacheDir </> remoteFrameworkUploadPath
      let dSYMLocalCachePath = cacheDir </> remotedSYMUploadPath

      when skipLocalCache $ do
        eitherFrameworkBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remoteFrameworkUploadPath fwn
        case eitherFrameworkBinary of
          Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> errorString e
          Right frameworkBinary -> unzipBinary frameworkBinary fwn frameworkZipName verbose

      unless skipLocalCache $ do
        frameworkExistsInLocalCache <- liftIO . doesFileExist $ frameworkLocalCachePath

        when frameworkExistsInLocalCache $ do
          sayFunc $ "Found " <> fwn <> " in local cache at: " <> frameworkLocalCachePath
          binary <- runResourceT $ C.sourceFile frameworkLocalCachePath C.$$ C.sinkLbs
          unzipBinary binary fwn frameworkZipName verbose

        unless frameworkExistsInLocalCache $ do
          eitherFrameworkBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remoteFrameworkUploadPath fwn
          case eitherFrameworkBinary of
            Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> errorString e
            Right frameworkBinary -> do
              saveBinaryToLocalCache cacheDir frameworkBinary remoteFrameworkUploadPath fwn verbose
              unzipBinary frameworkBinary fwn frameworkZipName verbose

      when skipLocalCache $ do
        eitherdSYMBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotedSYMUploadPath dSYMName
        case eitherdSYMBinary of
          Left e -> sayFunc $ "Error downloading " <> dSYMName <> " : " <> errorString e
          Right dSYMBinary -> unzipBinary dSYMBinary fwn dSYMZipName verbose

      unless skipLocalCache $ do
        dSYMExistsInLocalCache <- liftIO . doesFileExist $ dSYMLocalCachePath

        when dSYMExistsInLocalCache $ do
          sayFunc $ "Found " <> dSYMName <> " in local cache at: " <> dSYMLocalCachePath
          binary <- runResourceT $ C.sourceFile dSYMLocalCachePath C.$$ C.sinkLbs
          unzipBinary binary fwn dSYMZipName verbose

        unless dSYMExistsInLocalCache $ do
          eitherdSYMBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotedSYMUploadPath dSYMName
          case eitherdSYMBinary of
            Left e -> sayFunc $ "Error downloading " <> dSYMName <> " : " <> errorString e
            Right dSYMBinary -> do
              saveBinaryToLocalCache cacheDir dSYMBinary remotedSYMUploadPath dSYMName verbose
              unzipBinary dSYMBinary fwn dSYMZipName verbose

    Nothing -> do
      eitherFrameworkBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remoteFrameworkUploadPath fwn
      case eitherFrameworkBinary of
        Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> errorString e
        Right frameworkBinary -> unzipBinary frameworkBinary fwn frameworkZipName verbose

      eitherdSYMBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotedSYMUploadPath (fwn ++ ".dSYM")
      case eitherdSYMBinary of
        Left e -> sayFunc $ "Error downloading " <> dSYMName <> " : " <> errorString e
        Right dSYMBinary -> unzipBinary dSYMBinary fwn dSYMZipName verbose

  where
    s3BucketName = S3.BucketName bucketName
    frameworkZipName = frameworkArchiveName f version
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    dSYMZipName = dSYMArchiveName f version
    remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMName = fwn ++ ".dSYM"


downloadBinary s3BucketName objectRemotePath objectName = do
  readerEnv@(env{-, shouldVerify-}, _, verbose) <- ask
  runResourceT . AWS.runAWS env $ do
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose $
      sayFunc $ "Started downloading " <> objectName <> " from: " <> objectRemotePath
    rs <- AWS.send $ S3.getObject s3BucketName objectKey
    let cotentLength = fromMaybe 0 (view S3.gorsContentLength rs)
    binary <- view S3.gorsBody rs `AWS.sinkBody` sink verbose cotentLength
    sayFunc $ "Downloaded " <> objectName <> " from: " <> objectRemotePath
    return binary

  where
    objectKey = S3.ObjectKey . T.pack $ objectRemotePath
    sink verbose totalLength = if verbose then printProgress objectName totalLength C.=$= C.sinkLbs else C.sinkLbs

    printProgress :: MonadIO m => String -> Int -> C.Conduit BS.ByteString m BS.ByteString
    printProgress objectName totalLength = loop totalLength 0 0
      where
        roundedSizeInMB = bytesToMegaBytesRouded totalLength
        loop t consumedLen lastLen = C.await >>= maybe (return ()) (\bs -> do
            let len = consumedLen + BS.length bs
            let diffGreaterThan1MB = len - lastLen >= 1024*1024
            when ( diffGreaterThan1MB || len == t) $
               sayLnWithTime $ "Downloaded " ++ show (bytesToMegaBytesRouded len) ++ " MB of " ++ show roundedSizeInMB ++ " MB for " ++ objectName
            C.yield bs
            let a = if diffGreaterThan1MB then len else lastLen
            loop t len a)

bytesToMegaBytesRouded :: Integral a => a -> Double
bytesToMegaBytesRouded n = fromInteger (round (nInMB * (10^2))) / (10.0^^2)
  where
    nInMB = fromIntegral n / (1024*1024)

unzipBinary :: MonadIO m => LBS.ByteString -> String -> String -> Bool -> m ()
unzipBinary objectBinary objectName objectZipName verbose = do
  when verbose $
   sayLnWithTime $ "Staring to unzip " <> objectZipName
  liftIO $ Zip.extractFilesFromArchive (zipOptions False) (Zip.toArchive objectBinary)
  when verbose $
    sayLnWithTime $ "Unzipped " <> objectName <> " from: " <> objectZipName

remoteFrameworkPath :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> Version -> String
remoteFrameworkPath p r f v = remoteCacheDirectory p r f ++ frameworkArchiveName f v

remoteDsymPath :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> Version -> String
remoteDsymPath p r f v = remoteCacheDirectory p r f ++ dSYMArchiveName f v

remoteCacheDirectory :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> String
remoteCacheDirectory p r f = repoName </> show p ++ "/"
  where
    repoName = unGitRepoName $ repoNameForFrameworkName r f

probeCachesForFrameworks :: RomeCacheInfo -> InvertedRepositoryMap -> [FrameworkVersion] -> [TargetPlatform] -> ReaderT (AWS.Env, Bool) IO [FrameworkAvailability]
probeCachesForFrameworks cacheInfo reverseRomeMap frameworkVersions = sequence . probeForEachFramework
  where
    probeForEachFramework = mapM (probeCachesForFramework cacheInfo reverseRomeMap) frameworkVersions

probeCachesForFramework :: RomeCacheInfo -> InvertedRepositoryMap -> FrameworkVersion -> [TargetPlatform] -> ReaderT (AWS.Env, Bool) IO FrameworkAvailability
probeCachesForFramework cacheInfo reverseRomeMap frameworkVersion platforms = fmap (FrameworkAvailability frameworkVersion) probeForEachPlatform
  where
    probeForEachPlatform = mapM (probeCachesForFrameworkOnPlatform cacheInfo reverseRomeMap frameworkVersion) platforms

probeCachesForFrameworkOnPlatform :: RomeCacheInfo -> InvertedRepositoryMap -> FrameworkVersion -> TargetPlatform -> ReaderT (AWS.Env, Bool) IO PlatformAvailability
probeCachesForFrameworkOnPlatform (RomeCacheInfo bucketName localCacheDir) reverseRomeMap (FrameworkVersion fwn v) platform = do
  (env, verbose) <- ask
  let isAvailable = runResourceT . AWS.runAWS env $ checkIfFrameworkExistsInBucket s3BucketName frameworkObjectKey verbose
  PlatformAvailability platform <$> isAvailable
  where
    s3BucketName = S3.BucketName bucketName
    frameworkZipName = frameworkArchiveName fwn v
    frameworkObjectKey = S3.ObjectKey . T.pack $ remoteFrameworkPath platform reverseRomeMap fwn v

checkIfFrameworkExistsInBucket :: AWS.MonadAWS m => BucketName -> ObjectKey -> Bool -> m Bool
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

deriveFrameworkNamesAndVersion :: RepositoryMap -> [CartfileEntry] -> [FrameworkVersion]
deriveFrameworkNamesAndVersion romeMap = concatMap (deriveFrameworkNameAndVersion romeMap)

deriveFrameworkNameAndVersion ::  RepositoryMap -> CartfileEntry -> [FrameworkVersion]
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry GitHub (Location l) v) = map (`FrameworkVersion` v) $
  fromMaybe [FrameworkName gitHubRepositoryName] (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
  where
    gitHubRepositoryName = unGitRepoName $ gitRepoNameFromCartfileEntry cfe
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry Git (Location l) v)    = map (`FrameworkVersion` v) $
  fromMaybe [FrameworkName gitRepositoryName] (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
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

dSYMArchiveName :: FrameworkName -> Version -> String
dSYMArchiveName f (Version v) = appendFrameworkExtensionTo f ++ ".dSYM" ++ "-" ++ v ++ ".zip"

splitWithSeparator :: Char -> String -> [String]
splitWithSeparator a as = map T.unpack (T.split (== a) $ T.pack as)

getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities :: InvertedRepositoryMap -> [FrameworkAvailability] -> [GitRepoAvailability]
getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRomeMap = concatMap mergeRepoAvailabilities . groupAvailabilities . getGitRepoAvalabilities
  where
    getGitRepoAvalabilities :: [FrameworkAvailability] -> [GitRepoAvailability]
    getGitRepoAvalabilities = fmap getGitRepoAvailabilityFromFrameworkAvailability

    getGitRepoAvailabilityFromFrameworkAvailability :: FrameworkAvailability -> GitRepoAvailability
    getGitRepoAvailabilityFromFrameworkAvailability (FrameworkAvailability (FrameworkVersion fwn v) availabilities) = GitRepoAvailability (repoNameForFrameworkName reverseRomeMap fwn) v availabilities

    groupAvailabilities :: [GitRepoAvailability] -> [[GitRepoAvailability]]
    groupAvailabilities = groupBy ((==) `on` _availabilityRepo) . sortBy (compare `on` _availabilityRepo)

mergeRepoAvailabilities :: [GitRepoAvailability] -> [GitRepoAvailability]
mergeRepoAvailabilities repoAvailabilities@(x:xs) = [x { _repoPlatformAvailabilities = platformAvailabilities }]
  where
    sortAndGroupPlatformAvailabilities = groupBy ((==) `on` _availabilityPlatform) . sortBy (compare `on` _availabilityPlatform)
    groupedPlatformAvailabilities = sortAndGroupPlatformAvailabilities (repoAvailabilities >>= _repoPlatformAvailabilities)
    bothAvailable p p' = p { _isAvailable = _isAvailable p && _isAvailable p' }
    platformAvailabilities = fmap (foldl1 bothAvailable) groupedPlatformAvailabilities

formattedRepoAvailability :: ListMode -> GitRepoAvailability -> String
formattedRepoAvailability listMode r@(GitRepoAvailability (GitRepoName rn) (Version v) pas)
  | null filteredAvailabilities = ""
  | otherwise = unwords [rn, v, ":", formattedAvailabilities]
  where
    filteredAvailabilities = filterAccordingToListMode listMode pas
    formattedAvailabilities = unwords (formattedPlatformAvailability <$> filteredAvailabilities)


filterAccordingToListMode :: ListMode -> [PlatformAvailability] -> [PlatformAvailability]
filterAccordingToListMode All = id
filterAccordingToListMode Missing = filter (not . _isAvailable)
filterAccordingToListMode Present = filter _isAvailable

formattedPlatformAvailability :: PlatformAvailability -> String
formattedPlatformAvailability p = availabilityPrefix p ++ platformName p
  where
    availabilityPrefix (PlatformAvailability _ True) = "+"
    availabilityPrefix (PlatformAvailability _ False) = "-"
    platformName = show . _availabilityPlatform

s3ConfigFile :: MonadIO m => m FilePath
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

repoNameForFrameworkName :: InvertedRepositoryMap -> FrameworkName -> GitRepoName
repoNameForFrameworkName reverseRomeMap frameworkName = fromMaybe (GitRepoName . unFrameworkName $ frameworkName) (M.lookup frameworkName reverseRomeMap)

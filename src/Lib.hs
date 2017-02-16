{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



{- Exports -}
module Lib
    ( runRomeWithOptions
    , discoverRegion
    , filterByNameEqualTo
    , filterOutFrameworkNamesAndVersionsIfNotIn
    ) where



{- Imports -}
import qualified Codec.Archive.Zip            as Zip
import           Configuration
import           Control.Applicative          ((<|>))
import           Control.Lens                 hiding (List)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.Trans          (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Cartfile
import qualified Data.Conduit                 as C (Conduit, Sink, await, yield,
                                                    ($$), (=$=))
import qualified Data.Conduit.Binary          as C (sinkFile, sinkLbs,
                                                    sourceFile, sourceLbs)
import           Data.Function                (on)
import           Data.Ini                     as INI
import           Data.Ini.Utils               as INI
import           Data.List
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Romefile
import qualified Data.Text                    as T
import qualified Network.AWS                  as AWS
import           Network.AWS.Data
import           Network.AWS.S3               as S3
import           System.Directory
import           System.Environment
import           System.FilePath
import           Types
import           Types.Commands               as Commands
import           Types.TargetPlatform
import           Utils



runRomeWithOptions :: AWS.Env -> RomeOptions -> RomeMonad ()
runRomeWithOptions env (RomeOptions options verbose) = do
  cartfileEntries <- getCartfileEntires
  RomeFileParseResult { .. } <- getRomefileEntries
  let respositoryMap = toRepositoryMap repositoryMapEntries
  let reverseRepositoryMap = toInvertedRepositoryMap repositoryMapEntries
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
    zipDir dir verbose = liftIO $ Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive [dir]

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
      Left e -> sayFunc $ "Error uploading " <> objectName <> ": " <> awsErrorToString e
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
          Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> awsErrorToString e
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
            Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> awsErrorToString e
            Right frameworkBinary -> do
              saveBinaryToLocalCache cacheDir frameworkBinary remoteFrameworkUploadPath fwn verbose
              unzipBinary frameworkBinary fwn frameworkZipName verbose

      when skipLocalCache $ do
        eitherdSYMBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotedSYMUploadPath dSYMName
        case eitherdSYMBinary of
          Left e -> sayFunc $ "Error downloading " <> dSYMName <> " : " <> awsErrorToString e
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
            Left e -> sayFunc $ "Error downloading " <> dSYMName <> " : " <> awsErrorToString e
            Right dSYMBinary -> do
              saveBinaryToLocalCache cacheDir dSYMBinary remotedSYMUploadPath dSYMName verbose
              unzipBinary dSYMBinary fwn dSYMZipName verbose

    Nothing -> do
      eitherFrameworkBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remoteFrameworkUploadPath fwn
      case eitherFrameworkBinary of
        Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> awsErrorToString e
        Right frameworkBinary -> unzipBinary frameworkBinary fwn frameworkZipName verbose

      eitherdSYMBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotedSYMUploadPath (fwn ++ ".dSYM")
      case eitherdSYMBinary of
        Left e -> sayFunc $ "Error downloading " <> dSYMName <> " : " <> awsErrorToString e
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
        roundedSizeInMB = roundBytesToMegabytes totalLength
        loop t consumedLen lastLen = C.await >>= maybe (return ()) (\bs -> do
            let len = consumedLen + BS.length bs
            let diffGreaterThan1MB = len - lastLen >= 1024*1024
            when ( diffGreaterThan1MB || len == t) $
               sayLnWithTime $ "Downloaded " ++ show (roundBytesToMegabytes len) ++ " MB of " ++ show roundedSizeInMB ++ " MB for " ++ objectName
            C.yield bs
            let a = if diffGreaterThan1MB then len else lastLen
            loop t len a)

unzipBinary :: MonadIO m => LBS.ByteString -> String -> String -> Bool -> m ()
unzipBinary objectBinary objectName objectZipName verbose = do
  when verbose $
   sayLnWithTime $ "Staring to unzip " <> objectZipName
  liftIO $ Zip.extractFilesFromArchive [Zip.OptRecursive] (Zip.toArchive objectBinary)
  when verbose $
    sayLnWithTime $ "Unzipped " <> objectName <> " from: " <> objectZipName

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
filterAccordingToListMode Commands.All     = id
filterAccordingToListMode Commands.Missing = filter (not . _isAvailable)
filterAccordingToListMode Commands.Present = filter _isAvailable

discoverRegion :: RomeMonad AWS.Region
discoverRegion = do
  f <- getS3ConfigFile
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

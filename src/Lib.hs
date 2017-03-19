{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



{- Exports -}
module Lib where



{- Imports -}
import qualified Codec.Archive.Zip            as Zip
import           Configuration
import           Control.Lens                 hiding (List)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import qualified Data.Conduit                 as C (Conduit, await, yield, ($$),
                                                    (=$=))
import qualified Data.Conduit.Binary          as C (sinkFile, sinkLbs,
                                                    sourceFile, sourceLbs)
import           Data.Ini                     as INI
import           Data.Ini.Utils               as INI
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Romefile
import qualified Data.Text                    as T
import qualified Network.AWS                  as AWS
import qualified Network.AWS.Data             as AWS
import qualified Network.AWS.S3               as S3
import           System.Directory
import           System.Environment
import           System.FilePath
import           Types
import           Types.Commands               as Commands
import           Utils



-- | Runs Rome with `RomeOptions` on a given a `AWS.Env`.
runRomeWithOptions :: AWS.Env -- ^ The `AWS.Env` on which to run Rome.
                   -> RomeOptions -- ^ The `RomeOptions` to run Rome with.
                   -> RomeMonad ()
runRomeWithOptions env (RomeOptions options verbose) = do
  cartfileEntries <- getCartfileEntires
  RomeFileParseResult { .. } <- getRomefileEntries
  let respositoryMap = toRepositoryMap repositoryMapEntries
  let reverseRepositoryMap = toInvertedRepositoryMap repositoryMapEntries
  let ignoreNames = concatMap frameworkCommonNames ignoreMapEntries
  case options of

      Upload (RomeUDCPayload [] platforms {-shouldVerify-} shouldIgnoreLocalCache) -> do
        liftIO $ runReaderT (uploadFrameworksAndDsymsToCaches cacheInfo reverseRepositoryMap frameworkVersions platforms) readerEnv
        liftIO $ runReaderT (uploadVersionFilesToCaches cacheInfo gitRepoNamesAndVersions) readerEnv
        where
          frameworkVersions :: [FrameworkVersion]
          frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames

          gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
          gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions

          readerEnv :: (AWS.Env, SkipLocalCacheFlag, Bool)
          readerEnv = (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)

      Upload (RomeUDCPayload gitRepoNames platforms {-shouldVerify-} shouldIgnoreLocalCache) -> do
        liftIO $ runReaderT (uploadFrameworksAndDsymsToCaches cacheInfo reverseRepositoryMap frameworkVersions platforms) readerEnv
        liftIO $ runReaderT (uploadVersionFilesToCaches cacheInfo gitRepoNamesAndVersions) readerEnv
        where
          frameworkVersions :: [FrameworkVersion]
          frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames

          gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
          gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions

          readerEnv :: (AWS.Env, SkipLocalCacheFlag, Bool)
          readerEnv = (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)

      Download (RomeUDCPayload [] platforms {-shouldVerify-}  shouldIgnoreLocalCache) -> do
        liftIO $ runReaderT (downloadFrameworksAndDsymsFromCaches cacheInfo reverseRepositoryMap frameworkVersions platforms) readerEnv
        liftIO $ runReaderT (downloadVersionFilesToCaches cacheInfo gitRepoNamesAndVersions) readerEnv
        where
          frameworkVersions :: [FrameworkVersion]
          frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames

          gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
          gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions

          readerEnv :: (AWS.Env, SkipLocalCacheFlag, Bool)
          readerEnv = (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)


      Download (RomeUDCPayload gitRepoNames platforms {-shouldVerify-} shouldIgnoreLocalCache) -> do
        liftIO $ runReaderT (downloadFrameworksAndDsymsFromCaches cacheInfo reverseRepositoryMap frameworkVersions platforms) readerEnv
        liftIO $ runReaderT (downloadVersionFilesToCaches cacheInfo gitRepoNamesAndVersions) readerEnv
        where
          frameworkVersions :: [FrameworkVersion]
          frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames

          gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
          gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions

          readerEnv :: (AWS.Env, SkipLocalCacheFlag, Bool)
          readerEnv = (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)

      List (RomeListPayload listMode platforms) -> do
        availabilities <- liftIO $ runReaderT (probeCachesForFrameworks cacheInfo reverseRepositoryMap frameworkVersions platforms) (env, verbose)
        let repoAvailabilities = getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRepositoryMap availabilities
        let repoLines = filter (not . null) $ fmap (formattedRepoAvailability listMode) repoAvailabilities
        mapM_ sayLn repoLines
        where
          frameworkVersions :: [FrameworkVersion]
          frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames


-- | Uploads VersionFiles to a cache specified as `RomeCacheInfo`
-- | given a list of `GitRepoNameAndVersion`
uploadVersionFilesToCaches :: RomeCacheInfo -- ^ The chache definition.
                           -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoName` and `Version` information.
                           -> ReaderT UDCEnv IO ()
uploadVersionFilesToCaches cacheInfo = mapM_ (uploadVersionFileToCaches cacheInfo)



-- | Uploads one VersionFile from a cache specified as `RomeCacheInfo`
-- | given a `GitRepoNameAndVersion`
uploadVersionFileToCaches :: RomeCacheInfo -- ^ The chache definition.
                          -> GitRepoNameAndVersion -- ^ The `GitRepoName` and `Version` information.
                          -> ReaderT UDCEnv IO ()
uploadVersionFileToCaches (RomeCacheInfo bucketName localCacheDir) gitRepoNameAndVersion  = do
  (env {-, shouldVerify-}, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  versionFileExists <- liftIO $ doesFileExist versionFileLocalPath

  when versionFileExists $ do
     versionFileContent <- liftIO $ LBS.readFile versionFileLocalPath
     runMaybeT $
       MaybeT (return localCacheDir)
         >>= \dir -> liftIO $
                       unless skipLocalCache $ saveBinaryToLocalCache dir versionFileContent versionFileRemotePath versionFileName verbose
     runReaderT (uploadBinary s3BucketName versionFileContent versionFileRemotePath versionFileName) (env, verbose)

  where

    s3BucketName = S3.BucketName bucketName
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion


-- | Uploads a list of `FrameworkVersion` from which it derives dSYMs to a cache specified as `RomeCacheInfo`.
uploadFrameworksAndDsymsToCaches :: RomeCacheInfo -- ^ The chache definition.
                                 -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                 -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to upload.
                                 -> [TargetPlatform] -- ^ A list of target platforms restricting the scope of this action.
                                 -> ReaderT UDCEnv IO ()
uploadFrameworksAndDsymsToCaches cacheInfo reverseRomeMap fvs = mapM_ (sequence . uploadFramework)
  where
    uploadFramework = mapM (uploadFrameworkAndDsymToCaches cacheInfo reverseRomeMap) fvs



-- | Uploads a `FrameworkVersion` from which it derives dSYMs to a cache specified as `RomeCacheInfo`.
uploadFrameworkAndDsymToCaches :: RomeCacheInfo -- ^ The chache definition.
                               -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                               -> FrameworkVersion -- ^ The `FrameworkVersion` to upload.
                               -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                               -> ReaderT UDCEnv IO ()
uploadFrameworkAndDsymToCaches  (RomeCacheInfo bucketName localCacheDir) reverseRomeMap (FrameworkVersion f@(FrameworkName fwn) version) platform = do
  (env {-, shouldVerify-}, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  frameworkExists <- liftIO $ doesDirectoryExist frameworkDirectory
  dSymExists <- liftIO $ doesDirectoryExist dSYMdirectory

  when frameworkExists $ do
    when verbose $
      sayLnWithTime $ "Staring to zip: " <> frameworkDirectory
    frameworkArchive <- zipDir frameworkDirectory
    runMaybeT $
      MaybeT (return localCacheDir)
        >>= \dir -> liftIO $
                      unless skipLocalCache $ saveBinaryToLocalCache dir (Zip.fromArchive frameworkArchive) remoteFrameworkUploadPath fwn verbose
    runReaderT (uploadBinary s3BucketName (Zip.fromArchive frameworkArchive) remoteFrameworkUploadPath fwn) (env, verbose)

  when dSymExists $ do
    when verbose $
      sayLnWithTime $ "Staring to zip: " <> dSYMdirectory
    dSYMArchive <- zipDir dSYMdirectory
    runMaybeT $
      MaybeT (return localCacheDir)
        >>= \dir -> liftIO $
                      unless skipLocalCache $ saveBinaryToLocalCache dir (Zip.fromArchive dSYMArchive) remoteDsymUploadPath dSYMNameWithDSYMExtension verbose
    runReaderT (uploadBinary s3BucketName (Zip.fromArchive dSYMArchive) remoteDsymUploadPath (fwn ++ ".dSYM")) (env, verbose)

  where

    s3BucketName = S3.BucketName bucketName
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    dSYMNameWithDSYMExtension = frameworkNameWithFrameworkExtension ++ ".dSYM"
    dSYMdirectory = platformBuildDirectory </> dSYMNameWithDSYMExtension
    remoteDsymUploadPath = remoteDsymPath platform reverseRomeMap f version
    zipDir dir = liftIO $ Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive [dir]



-- | Uploads an artificat to an `S3.BucketName` at a given path in the bucket.
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



-- | Saves a ByteString to file in a given base directory.
saveBinaryToLocalCache :: MonadIO m
                       => FilePath -- ^ The path of the base directory.
                       -> LBS.ByteString -- ^ The `ByteString` to save.
                       -> FilePath -- ^ The destination path inised the base directory.
                       -> String -- ^ A colloquial name for the artifact printed when verbose is `True`.
                       -> Bool -- ^ A verbostiry flag.
                       -> m ()
saveBinaryToLocalCache cachePath binaryZip destinationPath objectName verbose = do
  when verbose $
    sayLnWithTime $ "Copying " <> objectName <> " to: " <> finalPath
  liftIO $ saveBinaryToFile binaryZip finalPath
  where
    finalPath = cachePath </> destinationPath


-- | Saves a ByteString to file
saveBinaryToFile :: MonadIO m
                 => LBS.ByteString -- ^ The `ByteString` to save.
                 -> FilePath -- ^ The destination path.
                 -> m ()
saveBinaryToFile binaryArtifact destinationPath = do
  liftIO $ createDirectoryIfMissing True (dropFileName destinationPath)
  liftIO . runResourceT $ C.sourceLbs binaryArtifact C.$$ C.sinkFile destinationPath


-- | Downloads VersionFiles from a cache specified as `RomeCacheInfo`
-- | given a list of `GitRepoNameAndVersion`
downloadVersionFilesToCaches :: RomeCacheInfo -- ^ The chache definition.
                             -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoName`s and `Version`s information.
                             -> ReaderT UDCEnv IO ()
downloadVersionFilesToCaches cacheInfo = mapM_ (downloadVersionFileToCaches cacheInfo)


-- | Downloads one VersionFile from a cache specified as `RomeCacheInfo`
-- | given a `GitRepoNameAndVersion`
downloadVersionFileToCaches :: RomeCacheInfo -- ^ The chache definition.
                            -> GitRepoNameAndVersion -- ^ The `GitRepoName` and `Version` information.
                            -> ReaderT UDCEnv IO ()
downloadVersionFileToCaches (RomeCacheInfo bucketName localCacheDir) gitRepoNameAndVersion  = do
  (_ {-, shouldVerify-}, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn

  case localCacheDir of
    Just cacheDir -> do
      when skipLocalCache $ do
        eitherdVersionFileBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName versionFileRemotePath versionFileName
        case eitherdVersionFileBinary of
          Left e -> sayFunc $ "Error downloading " <> versionFileName <> " : " <> awsErrorToString e
          Right versionFileBinary -> saveBinaryToFile versionFileBinary versionFileLocalPath

      unless skipLocalCache $ do
        let versionFileLocalCalchePath = cacheDir </> versionFileRemotePath
        versionFileExistsInLocalCache <- liftIO . doesFileExist $ versionFileLocalCalchePath

        when versionFileExistsInLocalCache $ do
          sayFunc $ "Found " <> versionFileName <> " in local cache at: " <> versionFileLocalCalchePath
          versionFileBinary <- runResourceT $ C.sourceFile versionFileLocalCalchePath C.$$ C.sinkLbs
          saveBinaryToFile versionFileBinary versionFileLocalPath

        unless versionFileExistsInLocalCache $ do
          eitherdVersionFileBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName versionFileRemotePath versionFileName
          case eitherdVersionFileBinary of
            Left e -> sayFunc $ "Error downloading " <> versionFileName <> " : " <> awsErrorToString e
            Right versionFileBinary -> do
              saveBinaryToLocalCache cacheDir versionFileBinary versionFileRemotePath versionFileName verbose
              saveBinaryToFile versionFileBinary versionFileLocalPath

    Nothing -> do
      eitherdVersionFileBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName versionFileRemotePath versionFileName
      case eitherdVersionFileBinary of
        Left e -> sayFunc $ "Error downloading " <> versionFileName <> " : " <> awsErrorToString e
        Right versionFileBinary -> saveBinaryToFile versionFileBinary versionFileLocalPath

  where
    s3BucketName = S3.BucketName bucketName
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName



-- | Downloads a list `FrameworkVersion` from which it derives dSYMs from a cache specified as `RomeCacheInfo`.
downloadFrameworksAndDsymsFromCaches :: RomeCacheInfo -- ^ The chache definition.
                                     -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                     -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to download.
                                     -> [TargetPlatform] -- ^ A list of target platforms restricting the scope of this action.
                                     -> ReaderT UDCEnv IO ()
downloadFrameworksAndDsymsFromCaches cacheInfo reverseRomeMap fvs = mapM_ (sequence . downloadFramework)
  where
    downloadFramework = mapM (downloadFrameworkAndDsymFromCaches cacheInfo reverseRomeMap) fvs



-- | Downloads a `FrameworkVersion` from which it derives dSYMs from a cache specified as `RomeCacheInfo`.
downloadFrameworkAndDsymFromCaches :: RomeCacheInfo -- ^ The chache definition.
                                   -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                   -> FrameworkVersion -- ^ The `FrameworkVersion` to download.
                                   -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                                   -> ReaderT UDCEnv IO ()
downloadFrameworkAndDsymFromCaches (RomeCacheInfo bucketName localCacheDir) reverseRomeMap (FrameworkVersion f@(FrameworkName fwn) version) platform = do
  (_{-, shouldVerify-}, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
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



-- | Downloads an artificat stored at a given path from an `S3.BucketName`.
downloadBinary s3BucketName objectRemotePath objectName = do
  (env{-, shouldVerify-}, _, verbose) <- ask
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
    printProgress objName totalLength = loop totalLength 0 0
      where
        roundedSizeInMB = roundBytesToMegabytes totalLength
        loop t consumedLen lastLen = C.await >>= maybe (return ()) (\bs -> do
            let len = consumedLen + BS.length bs
            let diffGreaterThan1MB = len - lastLen >= 1024*1024
            when ( diffGreaterThan1MB || len == t) $
               sayLnWithTime $ "Downloaded " ++ show (roundBytesToMegabytes len) ++ " MB of " ++ show roundedSizeInMB ++ " MB for " ++ objName
            C.yield bs
            let a = if diffGreaterThan1MB then len else lastLen
            loop t len a)



-- | Unzips a zipped (as in zip compression) `LBS.ByteString` in the current directory.
unzipBinary :: MonadIO m
            => LBS.ByteString -- ^ `LBS.The ByteString`.
            -> String -- ^ A colloquial name for the `LBS.ByteString` printed when verbose is `True`.
            -> String -- ^ A colloquial name for the artifact printed when verbose is `True`. Does not influence the artifact's name on disk.
            -> Bool -- ^ A verbostiry flag.
            -> m ()
unzipBinary objectBinary objectName objectZipName verbose = do
  when verbose $
   sayLnWithTime $ "Staring to unzip " <> objectZipName
  liftIO $ Zip.extractFilesFromArchive [Zip.OptRecursive] (Zip.toArchive objectBinary)
  when verbose $
    sayLnWithTime $ "Unzipped " <> objectName <> " from: " <> objectZipName



-- | Probes the caches described by `RomeCacheInfo` to check whether a list of `FrameworkVersion` is present or not
-- | in the caches for each `TargetPlatform`
probeCachesForFrameworks :: RomeCacheInfo -- ^ The chache definition.
                         -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                         -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
                         -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                         -> ReaderT (AWS.Env, Bool) IO [FrameworkAvailability]
probeCachesForFrameworks cacheInfo reverseRomeMap frameworkVersions = sequence . probeForEachFramework
  where
    probeForEachFramework = mapM (probeCachesForFramework cacheInfo reverseRomeMap) frameworkVersions



-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not in each `TargetPlatform`
probeCachesForFramework :: RomeCacheInfo -- ^ The chache definition.
                        -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                        -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                        -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                        -> ReaderT (AWS.Env, Bool) IO FrameworkAvailability
probeCachesForFramework cacheInfo reverseRomeMap frameworkVersion platforms = fmap (FrameworkAvailability frameworkVersion) probeForEachPlatform
  where
    probeForEachPlatform = mapM (probeCachesForFrameworkOnPlatform cacheInfo reverseRomeMap frameworkVersion) platforms


-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not for a `TargetPlatform`.
probeCachesForFrameworkOnPlatform :: RomeCacheInfo -- ^ The chache definition.
                                  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                                  -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                                  -> ReaderT (AWS.Env, Bool) IO PlatformAvailability
probeCachesForFrameworkOnPlatform (RomeCacheInfo bucketName _) reverseRomeMap (FrameworkVersion fwn v) platform = do
  (env, _) <- ask
  let isAvailable = runResourceT . AWS.runAWS env $ checkIfFrameworkExistsInBucket s3BucketName frameworkObjectKey
  PlatformAvailability platform <$> isAvailable
  where
    s3BucketName = S3.BucketName bucketName
    frameworkObjectKey = S3.ObjectKey . T.pack $ remoteFrameworkPath platform reverseRomeMap fwn v



-- | Probes a `S3.BucketName` to check whether an `S3.ObjectKey` is present or not.
checkIfFrameworkExistsInBucket :: AWS.MonadAWS m
                               => S3.BucketName -- ^ The name of the bucket.
                               -> S3.ObjectKey -- ^ The `S3.ObjectKey` to look for.
                               -> m Bool
checkIfFrameworkExistsInBucket s3BucketName frameworkObjectKey = do
  rs <- AWS.trying AWS._Error (AWS.send $ S3.headObject s3BucketName frameworkObjectKey)
  case rs of
    Left  _ -> return False
    Right _ -> return True



-- | Given a `ListMode` and a `GitRepoAvailability` produces a `String`
-- describing the `GitRepoAvailability` for a given `ListMode`.
formattedRepoAvailability :: ListMode -- ^ A given `ListMode`.
                          -> GitRepoAvailability -- ^ A given `GitRepoAvailability`.
                          -> String
formattedRepoAvailability listMode (GitRepoAvailability (GitRepoName rn) (Version v) pas)
  | null filteredAvailabilities = ""
  | otherwise = unwords [rn, v, ":", formattedAvailabilities]
  where
    filteredAvailabilities = filterAccordingToListMode listMode pas
    formattedAvailabilities = unwords (formattedPlatformAvailability <$> filteredAvailabilities)



-- | Filters a list of `PlatformAvailability` according to a `ListMode`
filterAccordingToListMode :: ListMode -- ^ A given `ListMode`
                          -> [PlatformAvailability] -- ^ A given list of `PlatformAvailability`
                          -> [PlatformAvailability]
filterAccordingToListMode Commands.All     = id
filterAccordingToListMode Commands.Missing = filter (not . _isAvailable)
filterAccordingToListMode Commands.Present = filter _isAvailable



-- | Discovers which `AWS.Region` to use by looking either at the _AWS_PROFILE_ environment variable
-- | or by falling back to using _default_. The region is then read from `Configuration.getS3ConfigFile`.
discoverRegion :: RomeMonad AWS.Region
discoverRegion = do
  f <- getS3ConfigFile
  profile <- liftIO $ lookupEnv "AWS_PROFILE"
  getRegionFromFile f (fromMaybe "default" profile)



-- | Reads a `AWS.Region` from file for a given profile
getRegionFromFile :: FilePath -- ^ The path to the file containing the `AWS.Region`
                  -> String -- ^ The name of the profile to use
                  -> RomeMonad AWS.Region
getRegionFromFile f profile = do
  i <- liftIO (INI.readIniFile f)
  case i of
    Left e -> throwError e
    Right ini -> do
      region <- withExceptT (\e -> "Could not parse " <> f <> ": " <> T.unpack e) $ INI.requireKey "region" `INI.inRequiredSection` T.pack profile `INI.fromIni''` ini
      let eitherAWSRegion = AWS.fromText region :: Either String AWS.Region
      case eitherAWSRegion of
        Left e  -> throwError e
        Right r -> return r

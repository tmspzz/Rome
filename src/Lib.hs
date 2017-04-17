{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}



{- Exports -}
module Lib where



{- Imports -}
import qualified Codec.Archive.Zip            as Zip
import           Configuration
import           Control.Lens                 hiding (List)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import qualified Data.Conduit                 as C (Conduit, await, yield, ($$),
                                                    (=$=))
import qualified Data.Conduit.Binary          as C (sinkFile, sinkLbs,
                                                    sourceFile, sourceLbs)
import           Data.Foldable
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
import qualified Turtle
import           Types
import           Types.Commands               as Commands
import           Utils



getAWSRegion :: (MonadIO m, MonadCatch m) => ExceptT String m AWS.Env
getAWSRegion = discoverRegion >>= flip AWS.newEnv AWS.Discover


bothCacheKeysMissingMessage :: String
bothCacheKeysMissingMessage = "Error: expected at least one of \"local\" or \
  \\"S3-Bucket\" key in the [Cache] section of your Romefile."

conflictingSkipLocalCacheOptionMessage :: String
conflictingSkipLocalCacheOptionMessage = "Error: only \"local\" key is present \
  \in the [Cache] section of your Romefile but you have asked Rome to skip \
  \this cache."

-- | Runs Rome with `RomeOptions` on a given a `AWS.Env`.
runRomeWithOptions :: RomeOptions -- ^ The `RomeOptions` to run Rome with.
                   -> RomeMonad ()
runRomeWithOptions (RomeOptions options verbose) = do
  cartfileEntries <- getCartfileEntires
  romeFileParseResult <- getRomefileEntries

  let respositoryMap = toRepositoryMap $ romeFileParseResult^.repositoryMapEntries
  let reverseRepositoryMap = toInvertedRepositoryMap $ romeFileParseResult^.repositoryMapEntries
  let ignoreNames = concatMap frameworkCommonNames $ romeFileParseResult^.ignoreMapEntries

  let cInfo = romeFileParseResult^.cacheInfo
  let mS3Bucket = cInfo^.bucket
  let mS3BucketName = S3.BucketName <$> mS3Bucket
  let mlCacheDir = cInfo^.localCacheDir

  case options of

      Upload (RomeUDCPayload gitRepoNames platforms skipLocalCache) ->

        if null gitRepoNames
          then
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames in
              uploadArtifacts (skipLocalCache, verbose) mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms
          else
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames in
              uploadArtifacts (skipLocalCache, verbose) mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms

      Download (RomeUDCPayload gitRepoNames platforms {-shouldVerify-}  skipLocalCache) ->
        if null gitRepoNames
          then
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames in
              downloadArtifacts (skipLocalCache, verbose) mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms
          else
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames in
              downloadArtifacts (skipLocalCache, verbose) mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms

        -- case (mS3Bucket, mlCacheDir) of
        --   (Just b,  l)   -> do
        --     env <- getAWSRegion
        --     let readerEnv = (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)
        --     liftIO $ runReaderT (downloadFrameworksAndDsymsFromCaches (S3.BucketName b) l reverseRepositoryMap frameworkVersions platforms) readerEnv
        --     liftIO $ runReaderT (downloadVersionFilesToCaches  (S3.BucketName b) l gitRepoNamesAndVersions) readerEnv
        --   (Nothing, Just l)   -> undefined
        --   (Nothing, Nothing)  -> throwError bothCacheKeysMissingMessage
        -- where
        --   frameworkVersions :: [FrameworkVersion]
        --   frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        --
        --   gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
        --   gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions



      -- Download (RomeUDCPayload gitRepoNames platforms {-shouldVerify-} shouldIgnoreLocalCache) ->

        -- case (mS3Bucket, mlCacheDir) of
        --   (Just b,  l)   -> do
        --     env <- getAWSRegion
        --     let readerEnv = (env{-, shouldVerify-}, shouldIgnoreLocalCache, verbose)
        --     liftIO $ runReaderT (downloadFrameworksAndDsymsFromCaches (S3.BucketName b) l reverseRepositoryMap frameworkVersions platforms) readerEnv
        --     liftIO $ runReaderT (downloadVersionFilesToCaches  (S3.BucketName b) l gitRepoNamesAndVersions) readerEnv
        --   (Nothing, Just l)   -> undefined
        --   (Nothing, Nothing)  -> throwError bothCacheKeysMissingMessage
        -- where
        --   frameworkVersions :: [FrameworkVersion]
        --   frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames
        --
        --   gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
        --   gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions


      List (RomeListPayload listMode platforms) ->
        case (mS3Bucket, mlCacheDir) of
          (Just b,  l)   -> do
            env <- getAWSRegion
            let readerEnv = (env{-, shouldVerify-}, verbose)
            availabilities <- liftIO $ runReaderT (probeCachesForFrameworks (S3.BucketName b) l reverseRepositoryMap frameworkVersions platforms) readerEnv
            let repoAvailabilities = getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRepositoryMap availabilities
            let repoLines = filter (not . null) $ fmap (formattedRepoAvailability listMode) repoAvailabilities
            mapM_ sayLn repoLines
          (Nothing, Just l)   -> undefined
          (Nothing, Nothing)  -> throwError bothCacheKeysMissingMessage
        where
          frameworkVersions :: [FrameworkVersion]
          frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames


downloadArtifacts :: (SkipLocalCacheFlag, Bool)
                  -> Maybe S3.BucketName
                  -> Maybe FilePath
                  -> InvertedRepositoryMap
                  -> [FrameworkVersion]
                  -> [TargetPlatform]
                  -> RomeMonad ()
downloadArtifacts (s@(SkipLocalCacheFlag skipLocalCache), verbose)
                  mS3BucketName
                  mlCacheDir
                  reverseRepositoryMap
                  frameworkVersions
                  platforms = undefined

uploadArtifacts :: (SkipLocalCacheFlag, Bool)
                -> Maybe S3.BucketName
                -> Maybe FilePath
                -> InvertedRepositoryMap
                -> [FrameworkVersion]
                -> [TargetPlatform]
                -> RomeMonad ()
uploadArtifacts (s@(SkipLocalCacheFlag skipLocalCache), verbose)
                mS3BucketName
                mlCacheDir
                reverseRepositoryMap
                frameworkVersions
                platforms =
  case (mS3BucketName, mlCacheDir) of
    (Just s3BucketName,  lCacheDir) -> do
      env <- getAWSRegion

      liftIO $ runReaderT (uploadFrameworksAndDsymsToCaches s3BucketName lCacheDir reverseRepositoryMap frameworkVersions platforms) (env, s, verbose)
      liftIO $ runReaderT (uploadVersionFilesToCaches s3BucketName lCacheDir gitRepoNamesAndVersions) (env, s, verbose)

    (Nothing, Just lCacheDir) -> do
      when skipLocalCache $
        throwError conflictingSkipLocalCacheOptionMessage
      liftIO $
        runReaderT (saveFrameworksAndDSYMsToLocalCache lCacheDir reverseRepositoryMap frameworkVersions platforms) verbose
        >> runReaderT (saveVersionFilesToLocalCache lCacheDir gitRepoNamesAndVersions) verbose


    (Nothing, Nothing)  -> throwError bothCacheKeysMissingMessage

    where

      gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
      gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions


saveVersionFilesToLocalCache :: FilePath
                             -> [GitRepoNameAndVersion]
                             -> ReaderT Bool IO ()
saveVersionFilesToLocalCache lCacheDir = mapM_ (saveVersonFileToLocalCache lCacheDir)



saveVersonFileToLocalCache :: FilePath
                           -> GitRepoNameAndVersion
                           -> ReaderT Bool IO ()
saveVersonFileToLocalCache lCacheDir
                           gitRepoNameAndVersion = do
  verbose <- ask
  versionFileExists <- liftIO $ doesFileExist versionFileLocalPath

  when versionFileExists $ do
    versionFileContent <- liftIO $ LBS.readFile versionFileLocalPath
    saveVersionFileBinaryToLocalCache lCacheDir versionFileContent gitRepoNameAndVersion verbose

  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName

-- | Uploads VersionFiles to a cache specified as `RomeCacheInfo`
-- | given a list of `GitRepoNameAndVersion`
uploadVersionFilesToCaches :: S3.BucketName -- ^ The chache definition.
                           -> Maybe FilePath
                           -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoName` and `Version` information.
                           -> ReaderT UDCEnv IO ()
uploadVersionFilesToCaches s3Bucket mlCacheDir = mapM_ (uploadVersionFileToCaches s3Bucket mlCacheDir)



-- | Uploads one VersionFile from a cache specified as `RomeCacheInfo`
-- | given a `GitRepoNameAndVersion`
uploadVersionFileToCaches :: S3.BucketName -- ^ The chache definition.
                          -> Maybe FilePath
                          -> GitRepoNameAndVersion -- ^ The `GitRepoName` and `Version` information.
                          -> ReaderT UDCEnv IO ()
uploadVersionFileToCaches s3BucketName
                          mlCacheDir
                          gitRepoNameAndVersion = do
  (env, SkipLocalCacheFlag skipLocalCache, verbose) <- ask

  versionFileExists <- liftIO $ doesFileExist versionFileLocalPath

  when versionFileExists $ do
     versionFileContent <- liftIO $ LBS.readFile versionFileLocalPath
     for_ mlCacheDir $ \lCacheDir ->
        unless skipLocalCache $
          saveVersionFileBinaryToLocalCache lCacheDir versionFileContent gitRepoNameAndVersion verbose
     liftIO $ runReaderT (uploadVersionFileToS3 s3BucketName versionFileContent gitRepoNameAndVersion) (env, verbose)

  where

    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName



uploadVersionFileToS3 :: S3.BucketName
                      -> LBS.ByteString
                      -> GitRepoNameAndVersion
                      -> ReaderT (AWS.Env, Bool) IO ()
uploadVersionFileToS3  s3BucketName
                       versionFileContent
                       gitRepoNameAndVersion =
  uploadBinary s3BucketName
               versionFileContent
               versionFileRemotePath
               versionFileName

  where

    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion




saveVersionFileBinaryToLocalCache :: MonadIO m
                                  => FilePath
                                  -> LBS.ByteString
                                  -> GitRepoNameAndVersion
                                  -> Bool
                                  -> m ()
saveVersionFileBinaryToLocalCache lCacheDir
                                  versionFileContent
                                  gitRepoNameAndVersion =
  saveBinaryToLocalCache lCacheDir
                         versionFileContent
                         versionFileRemotePath
                         versionFileName

  where

    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion



-- | Uploads a list of `FrameworkVersion` from which it derives dSYMs to a cache specified as `RomeCacheInfo`.
uploadFrameworksAndDsymsToCaches :: S3.BucketName -- ^ The chache definition.
                                 -> Maybe FilePath
                                 -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                 -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to upload.
                                 -> [TargetPlatform] -- ^ A list of target platforms restricting the scope of this action.
                                 -> ReaderT UDCEnv IO ()
uploadFrameworksAndDsymsToCaches s3BucketName
                                 mlCacheDir
                                 reverseRomeMap
                                 fvs = mapM_ (sequence . upload)
  where
    upload = mapM (uploadFrameworkAndDsymToCaches s3BucketName mlCacheDir reverseRomeMap) fvs



uploadFrameworkToS3 :: Zip.Archive
                    -> S3.BucketName
                    -> InvertedRepositoryMap
                    -> FrameworkVersion
                    -> TargetPlatform
                    -> ReaderT (AWS.Env, Bool) IO ()
uploadFrameworkToS3 frameworkArchive
                    s3BucketName
                    reverseRomeMap
                    (FrameworkVersion f@(FrameworkName fwn) version)
                    platform = do
  (env, verbose) <- ask
  runReaderT
    (uploadBinary s3BucketName (Zip.fromArchive frameworkArchive) remoteFrameworkUploadPath fwn)
    (env, verbose)

  where
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version


uploadDsymToS3 :: Zip.Archive
                    -> S3.BucketName
                    -> InvertedRepositoryMap
                    -> FrameworkVersion
                    -> TargetPlatform
                    -> ReaderT (AWS.Env, Bool) IO ()
uploadDsymToS3 dSYMArchive
               s3BucketName
               reverseRomeMap
               (FrameworkVersion f@(FrameworkName fwn) version)
               platform = do
  (env, verbose) <- ask
  runReaderT (uploadBinary s3BucketName (Zip.fromArchive dSYMArchive) remoteDsymUploadPath (fwn ++ ".dSYM")) (env, verbose)

  where
    remoteDsymUploadPath = remoteDsymPath platform reverseRomeMap f version



-- | Uploads a `FrameworkVersion` from which it derives dSYMs to a cache specified as `RomeCacheInfo`.
uploadFrameworkAndDsymToCaches :: S3.BucketName -- ^ The chache definition.
                               -> Maybe FilePath
                               -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                               -> FrameworkVersion -- ^ The `FrameworkVersion` to upload.
                               -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                               -> ReaderT UDCEnv IO ()
uploadFrameworkAndDsymToCaches s3BucketName
                               mlCacheDir
                               reverseRomeMap
                               fVersion@(FrameworkVersion f@(FrameworkName _) _)
                               platform = do
  (env, s@(SkipLocalCacheFlag skipLocalCache), verbose) <- ask

  void . runExceptT $ do
    frameworkArchive <- zipDir frameworkDirectory verbose
    for_ mlCacheDir $ \lCacheDir ->
           unless skipLocalCache $
            liftIO $ runReaderT
              (saveFrameworkToLocalCache lCacheDir frameworkArchive reverseRomeMap fVersion platform)
              (s, verbose)
    liftIO $ runReaderT
      (uploadFrameworkToS3 frameworkArchive s3BucketName reverseRomeMap fVersion platform)
      (env, verbose)

  void . runExceptT $ do
    dSYMArchive <- zipDir dSYMdirectory verbose
    for_ mlCacheDir $ \lCacheDir ->
           unless skipLocalCache $
            liftIO $ runReaderT
              (saveDsymToLocalCache lCacheDir dSYMArchive reverseRomeMap fVersion platform)
              (s, verbose)
    liftIO $ runReaderT
      (uploadDsymToS3 dSYMArchive s3BucketName reverseRomeMap fVersion platform)
      (env, verbose)

  where

    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension
    dSYMNameWithDSYMExtension = frameworkNameWithFrameworkExtension ++ ".dSYM"
    dSYMdirectory = platformBuildDirectory </> dSYMNameWithDSYMExtension



saveFrameworksAndDSYMsToLocalCache :: MonadIO m
                                   => FilePath
                                   -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                   -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to upload.
                                   -> [TargetPlatform] -- ^ A list of target platforms restricting the scope of this action.
                                   -> ReaderT Bool m ()
saveFrameworksAndDSYMsToLocalCache lCacheDir reverseRomeMap fvs = mapM_ (sequence . save)
  where
    save = mapM (saveFrameworkAndDSYMToLocalCache lCacheDir reverseRomeMap) fvs

saveFrameworkAndDSYMToLocalCache :: MonadIO m
                                 => FilePath
                                 -> InvertedRepositoryMap
                                 -> FrameworkVersion
                                 -> TargetPlatform
                                 -> ReaderT Bool m ()
saveFrameworkAndDSYMToLocalCache lCacheDir
                                 reverseRomeMap
                                 fVersion@(FrameworkVersion f@(FrameworkName _) _)
                                 platform = do
  verbose <- ask
  void . runExceptT $ do
    frameworkArchive <- zipDir frameworkDirectory verbose
    liftIO $
      runReaderT
        (saveFrameworkToLocalCache lCacheDir frameworkArchive reverseRomeMap fVersion platform)
        (SkipLocalCacheFlag False, verbose)

  void . runExceptT $ do
    dSYMArchive <- zipDir dSYMdirectory verbose
    liftIO $
      runReaderT
        (saveDsymToLocalCache lCacheDir dSYMArchive reverseRomeMap fVersion platform)
        (SkipLocalCacheFlag False, verbose)

  where
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension
    dSYMNameWithDSYMExtension = frameworkNameWithFrameworkExtension ++ ".dSYM"
    dSYMdirectory = platformBuildDirectory </> dSYMNameWithDSYMExtension




saveFrameworkToLocalCache :: FilePath
                          -> Zip.Archive
                          -> InvertedRepositoryMap
                          -> FrameworkVersion
                          -> TargetPlatform
                          -> ReaderT (SkipLocalCacheFlag, Bool) IO ()
saveFrameworkToLocalCache lCacheDir
                          frameworkArchive
                          reverseRomeMap
                          (FrameworkVersion f@(FrameworkName _) version)
                          platform = do
  (SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  unless skipLocalCache $
   saveBinaryToLocalCache lCacheDir
                          (Zip.fromArchive frameworkArchive)
                          remoteFrameworkUploadPath
                          frameworkNameWithFrameworkExtension
                          verbose

  where
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f



saveDsymToLocalCache :: FilePath
                      -> Zip.Archive
                      -> InvertedRepositoryMap
                      -> FrameworkVersion
                      -> TargetPlatform
                      -> ReaderT (SkipLocalCacheFlag, Bool) IO ()
saveDsymToLocalCache  lCacheDir
                      dSYMArchive
                      reverseRomeMap
                      (FrameworkVersion f@(FrameworkName fwn) version)
                      platform = do
  (SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  unless skipLocalCache $
   saveBinaryToLocalCache lCacheDir
                          (Zip.fromArchive dSYMArchive)
                          remoteDsymUploadPath
                          (fwn ++ ".dSYM")
                          verbose

  where
    remoteDsymUploadPath = remoteDsymPath platform reverseRomeMap f version



zipDir :: MonadIO m => FilePath -> Bool -> ExceptT String m Zip.Archive
zipDir dir verbose = do
  directoryExists <- liftIO $ doesDirectoryExist dir
  if directoryExists
    then do
      when verbose $
          sayLnWithTime $ "Staring to zip: " <> dir
      liftIO $ Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive [dir]
    else throwError $ "Error: " <> dir <> " does not exist"




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
downloadVersionFilesToCaches :: S3.BucketName -- ^ The chache definition.
                             -> Maybe FilePath
                             -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoName`s and `Version`s information.
                             -> ReaderT UDCEnv IO ()
downloadVersionFilesToCaches s3BucketName lDir = mapM_ (downloadVersionFileToCaches s3BucketName lDir)


-- | Downloads one VersionFile from a cache specified as `RomeCacheInfo`
-- | given a `GitRepoNameAndVersion`
downloadVersionFileToCaches :: S3.BucketName -- ^ The chache definition.
                            -> Maybe FilePath
                            -> GitRepoNameAndVersion -- ^ The `GitRepoName` and `Version` information.
                            -> ReaderT UDCEnv IO ()
downloadVersionFileToCaches s3BucketName lDir gitRepoNameAndVersion  = do
  (_ {-, shouldVerify-}, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn

  case lDir of
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
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName



-- | Downloads a list `FrameworkVersion` from which it derives dSYMs from a cache specified as `RomeCacheInfo`.
downloadFrameworksAndDsymsFromCaches :: S3.BucketName -- ^ The chache definition.
                                     -> Maybe FilePath
                                     -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                     -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to download.
                                     -> [TargetPlatform] -- ^ A list of target platforms restricting the scope of this action.
                                     -> ReaderT UDCEnv IO ()
downloadFrameworksAndDsymsFromCaches s3BucketName lDir reverseRomeMap fvs = mapM_ (sequence . downloadFramework)
  where
    downloadFramework = mapM (downloadFrameworkAndDsymFromCaches s3BucketName lDir reverseRomeMap) fvs



-- | Downloads a `FrameworkVersion` from which it derives dSYMs from a cache specified as `RomeCacheInfo`.
downloadFrameworkAndDsymFromCaches :: S3.BucketName -- ^ The chache definition.
                                   -> Maybe FilePath
                                   -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                   -> FrameworkVersion -- ^ The `FrameworkVersion` to download.
                                   -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                                   -> ReaderT UDCEnv IO ()
downloadFrameworkAndDsymFromCaches s3BucketName lDir reverseRomeMap (FrameworkVersion f@(FrameworkName fwn) version) platform = do
  (_{-, shouldVerify-}, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  case lDir of
    Just cacheDir -> do

      let frameworkLocalCachePath = cacheDir </> remoteFrameworkUploadPath
      let dSYMLocalCachePath = cacheDir </> remotedSYMUploadPath

      when skipLocalCache $ do
        eitherFrameworkBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remoteFrameworkUploadPath fwn
        case eitherFrameworkBinary of
          Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> awsErrorToString e
          Right frameworkBinary -> unzipBinary frameworkBinary fwn frameworkZipName verbose
                                   <* makeExecutable platform f

      unless skipLocalCache $ do
        frameworkExistsInLocalCache <- liftIO . doesFileExist $ frameworkLocalCachePath

        when frameworkExistsInLocalCache $ do
          sayFunc $ "Found " <> fwn <> " in local cache at: " <> frameworkLocalCachePath
          binary <- runResourceT $ C.sourceFile frameworkLocalCachePath C.$$ C.sinkLbs
          unzipBinary binary fwn frameworkZipName verbose <* makeExecutable platform f


        unless frameworkExistsInLocalCache $ do
          eitherFrameworkBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remoteFrameworkUploadPath fwn
          case eitherFrameworkBinary of
            Left e -> sayFunc $ "Error downloading " <> fwn <> " : " <> awsErrorToString e
            Right frameworkBinary -> do
              saveBinaryToLocalCache cacheDir frameworkBinary remoteFrameworkUploadPath fwn verbose
              unzipBinary frameworkBinary fwn frameworkZipName verbose <* makeExecutable platform f


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
                                 <* makeExecutable platform f


      eitherdSYMBinary <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotedSYMUploadPath (fwn ++ ".dSYM")
      case eitherdSYMBinary of
        Left e -> sayFunc $ "Error downloading " <> dSYMName <> " : " <> awsErrorToString e
        Right dSYMBinary -> unzipBinary dSYMBinary fwn dSYMZipName verbose


  where
    frameworkZipName = frameworkArchiveName f version
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    dSYMZipName = dSYMArchiveName f version
    remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMName = fwn ++ ".dSYM"

    makeExecutable :: MonadIO m => TargetPlatform -> FrameworkName -> m Turtle.Permissions
    makeExecutable p fname = Turtle.chmod Turtle.executable
                            (
                              Turtle.fromString $
                                frameworkBuildBundleForPlatform p fname
                                </> unFrameworkName fname
                            )



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
probeCachesForFrameworks :: S3.BucketName -- ^ The chache definition.
                         -> Maybe FilePath
                         -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                         -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
                         -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                         -> ReaderT (AWS.Env, Bool) IO [FrameworkAvailability]
probeCachesForFrameworks s3BucketName lDir reverseRomeMap frameworkVersions = sequence . probeForEachFramework
  where
    probeForEachFramework = mapM (probeCachesForFramework s3BucketName lDir reverseRomeMap) frameworkVersions



-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not in each `TargetPlatform`
probeCachesForFramework :: S3.BucketName -- ^ The chache definition.
                        -> Maybe FilePath
                        -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                        -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                        -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                        -> ReaderT (AWS.Env, Bool) IO FrameworkAvailability
probeCachesForFramework s3BucketName lDir reverseRomeMap frameworkVersion platforms = fmap (FrameworkAvailability frameworkVersion) probeForEachPlatform
  where
    probeForEachPlatform = mapM (probeCachesForFrameworkOnPlatform s3BucketName lDir reverseRomeMap frameworkVersion) platforms


-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not for a `TargetPlatform`.
probeCachesForFrameworkOnPlatform :: S3.BucketName -- ^ The chache definition.
                                  -> Maybe FilePath
                                  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                                  -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                                  -> ReaderT (AWS.Env, Bool) IO PlatformAvailability
probeCachesForFrameworkOnPlatform s3BucketName lDir reverseRomeMap (FrameworkVersion fwn v) platform = do
  (env, _) <- ask
  let isAvailable = runResourceT . AWS.runAWS env $ checkIfFrameworkExistsInBucket s3BucketName frameworkObjectKey
  PlatformAvailability platform <$> isAvailable
  where
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
discoverRegion :: MonadIO m => ExceptT String m AWS.Region
discoverRegion = do
  f <- getS3ConfigFile
  profile <- liftIO $ lookupEnv "AWS_PROFILE"
  getRegionFromFile f (fromMaybe "default" profile)



-- | Reads a `AWS.Region` from file for a given profile
getRegionFromFile :: MonadIO m
                  => FilePath -- ^ The path to the file containing the `AWS.Region`
                  -> String -- ^ The name of the profile to use
                  -> ExceptT String m AWS.Region
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

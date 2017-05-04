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
              runReaderT (uploadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms) (skipLocalCache, verbose)
          else
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames in
              runReaderT (uploadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms) (skipLocalCache, verbose)

      Download (RomeUDCPayload gitRepoNames platforms skipLocalCache) ->
        if null gitRepoNames
          then
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames in
              runReaderT (downloadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms) (skipLocalCache, verbose)
          else
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames in
              runReaderT (downloadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms) (skipLocalCache, verbose)

      List (RomeListPayload listMode platforms) ->
          let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` ignoreNames in
          runReaderT (listArtifacts mS3BucketName mlCacheDir listMode reverseRepositoryMap frameworkVersions platforms) (SkipLocalCacheFlag False, verbose)



listArtifacts :: Maybe S3.BucketName
              -> Maybe FilePath
              -> ListMode
              -> InvertedRepositoryMap
              -> [FrameworkVersion]
              -> [TargetPlatform]
              -> ReaderT (SkipLocalCacheFlag, Bool) RomeMonad ()
listArtifacts mS3BucketName
              mlCacheDir
              listMode
              reverseRepositoryMap
              frameworkVersions
              platforms = do
  (_, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  repoAvailabilities <- getRepoAvailabilityFromCaches mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms
  mapM_ sayFunc $ repoLines repoAvailabilities
  where
    repoLines repoAvailabilities = filter (not . null) $ fmap (formattedRepoAvailability listMode) repoAvailabilities



getRepoAvailabilityFromCaches :: Maybe S3.BucketName
                              -> Maybe FilePath
                              -> InvertedRepositoryMap
                              -> [FrameworkVersion]
                              -> [TargetPlatform]
                              -> ReaderT (SkipLocalCacheFlag, Bool) RomeMonad [GitRepoAvailability]
getRepoAvailabilityFromCaches (Just s3BucketName)
                              _
                              reverseRepositoryMap
                              frameworkVersions
                              platforms = do
  env <- lift getAWSRegion
  (_, verbose) <- ask
  let readerEnv = (env, verbose)
  availabilities <- liftIO $ runReaderT (probeS3ForFrameworks s3BucketName reverseRepositoryMap frameworkVersions platforms) readerEnv
  return $ getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRepositoryMap availabilities

getRepoAvailabilityFromCaches Nothing
                              (Just lCacheDir)
                              reverseRepositoryMap
                              frameworkVersions
                              platforms = do
  (SkipLocalCacheFlag skipLocalCache, _) <- ask
  when skipLocalCache $
    throwError conflictingSkipLocalCacheOptionMessage

  availabilities <- probeLocalCacheForFrameworks lCacheDir reverseRepositoryMap frameworkVersions platforms
  return $ getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRepositoryMap availabilities

getRepoAvailabilityFromCaches Nothing
                              Nothing
                              _
                              _
                              _ = throwError bothCacheKeysMissingMessage




downloadArtifacts :: Maybe S3.BucketName
                  -> Maybe FilePath
                  -> InvertedRepositoryMap
                  -> [FrameworkVersion]
                  -> [TargetPlatform]
                  -> ReaderT (SkipLocalCacheFlag, Bool) RomeMonad ()
downloadArtifacts mS3BucketName
                  mlCacheDir
                  reverseRepositoryMap
                  frameworkVersions
                  platforms = do
  (s@(SkipLocalCacheFlag skipLocalCache), verbose) <- ask

  case (mS3BucketName, mlCacheDir) of

    (Just s3BucketName,  lCacheDir) -> do
      env <- lift getAWSRegion
      liftIO $ runReaderT (downloadFrameworksAndDsymsFromCaches s3BucketName lCacheDir reverseRepositoryMap frameworkVersions platforms) (env, s, verbose)
      liftIO $ runReaderT (downloadVersionFilesFromCaches s3BucketName lCacheDir gitRepoNamesAndVersions) (env, s, verbose)

    (Nothing, Just lCacheDir) -> do
      when skipLocalCache $
        throwError conflictingSkipLocalCacheOptionMessage
      liftIO $ do
        runReaderT
          (do
            let sayFunc = if verbose then sayLnWithTime else sayLn
            errors <- mapM runExceptT $ getAndUnzipFrameworksAndDSYMsFromLocalCache lCacheDir reverseRepositoryMap frameworkVersions platforms
            mapM_ (whenError sayFunc) errors
          ) verbose
        runReaderT
          (do
            let sayFunc = if verbose then sayLnWithTime else sayLn
            errors <- mapM runExceptT $ getAndSaveVersionFilesFromLocalCache lCacheDir gitRepoNamesAndVersions
            mapM_ (whenError sayFunc) errors
          ) verbose

    (Nothing, Nothing)  -> throwError bothCacheKeysMissingMessage

    where

      gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
      gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions

uploadArtifacts :: Maybe S3.BucketName
                -> Maybe FilePath
                -> InvertedRepositoryMap
                -> [FrameworkVersion]
                -> [TargetPlatform]
                -> ReaderT (SkipLocalCacheFlag, Bool) RomeMonad ()
uploadArtifacts mS3BucketName
                mlCacheDir
                reverseRepositoryMap
                frameworkVersions
                platforms = do
  (s@(SkipLocalCacheFlag skipLocalCache), verbose) <- ask
  case (mS3BucketName, mlCacheDir) of
    (Just s3BucketName,  lCacheDir) -> do
      env <- lift getAWSRegion

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
     unless skipLocalCache $
      maybe (return ()) liftIO $
          saveVersionFileBinaryToLocalCache <$> mlCacheDir <*> Just versionFileContent <*> Just gitRepoNameAndVersion <*> Just verbose
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
    unless skipLocalCache $
      maybe (return ()) liftIO $
        runReaderT
          <$> (
              saveFrameworkToLocalCache
              <$> mlCacheDir
              <*> Just frameworkArchive
              <*> Just reverseRomeMap
              <*> Just fVersion
              <*> Just platform
              )
          <*> Just (s, verbose)
    liftIO $ runReaderT
      (uploadFrameworkToS3 frameworkArchive s3BucketName reverseRomeMap fVersion platform)
      (env, verbose)

  void . runExceptT $ do
    dSYMArchive <- zipDir dSYMdirectory verbose
    unless skipLocalCache $
      maybe (return ()) liftIO $
        runReaderT
          <$> (
              saveDsymToLocalCache
              <$> mlCacheDir
              <*> Just dSYMArchive
              <*> Just reverseRomeMap
              <*> Just fVersion
              <*> Just platform
              )
          <*> Just (s, verbose)
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
  let sayFunc = if verbose then sayLnWithTime else sayLn
  when verbose $
    sayLnWithTime $ "Copying " <> objectName <> " to: " <> finalPath
  liftIO $ saveBinaryToFile binaryZip finalPath
  sayFunc $ "Copied " <> objectName <> " to: " <> finalPath
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
downloadVersionFilesFromCaches :: S3.BucketName -- ^ The chache definition.
                               -> Maybe FilePath
                               -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoName`s and `Version`s information.
                               -> ReaderT UDCEnv IO ()
downloadVersionFilesFromCaches s3BucketName
                               lDir = mapM_ (downloadVersionFileFromCaches s3BucketName lDir)


-- | Downloads one VersionFile from a cache specified as `RomeCacheInfo`
-- | given a `GitRepoNameAndVersion`
downloadVersionFileFromCaches :: S3.BucketName -- ^ The chache definition.
                              -> Maybe FilePath
                              -> GitRepoNameAndVersion -- ^ The `GitRepoName` and `Version` information.
                              -> ReaderT UDCEnv IO ()
downloadVersionFileFromCaches s3BucketName (Just lCacheDir) gitRepoNameAndVersion = do
  (env, SkipLocalCacheFlag skipLocalCache, verbose) <- ask

  when skipLocalCache $
    downloadVersionFileFromCaches s3BucketName Nothing gitRepoNameAndVersion

  unless skipLocalCache $ do
    eitherSuccess <- runReaderT (runExceptT $ getAndSaveVersionFileFromLocalCache lCacheDir gitRepoNameAndVersion) verbose
    case eitherSuccess of
      Right _ -> return ()
      Left e -> liftIO $ do
        let sayFunc = if verbose then sayLnWithTime else sayLn
        sayFunc e
        runReaderT
          ( do
            let sayFunc2 = if verbose then sayLnWithTime else sayLn
            e2 <- runExceptT $ do
              let sayFunc3 = if verbose then sayLnWithTime else sayLn
              versionFileBinary <- getVersionFileFromS3 s3BucketName gitRepoNameAndVersion
              saveBinaryToLocalCache lCacheDir versionFileBinary versionFileRemotePath versionFileName verbose
              saveBinaryToFile versionFileBinary versionFileLocalPath
              sayFunc3 $ "Copied " <> versionFileName <> " to: " <> versionFileLocalPath
            whenError sayFunc2 e2
           ) (env, verbose)

  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion

downloadVersionFileFromCaches s3BucketName Nothing gitRepoNameAndVersion = do
  (env, _, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  eitherError <- liftIO $ runReaderT
                          (runExceptT $ do
                            let sayFunc2 = if verbose then sayLnWithTime else sayLn
                            versionFileBinary <- getVersionFileFromS3 s3BucketName gitRepoNameAndVersion
                            saveBinaryToFile versionFileBinary versionFileLocalPath
                            sayFunc2 $ "Copied " <> versionFileName <> " to: " <> versionFileLocalPath
                          )
                          (env, verbose)
  whenError sayFunc eitherError
  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName



getAndSaveVersionFilesFromLocalCache :: MonadIO m
                                     => FilePath
                                     -> [GitRepoNameAndVersion]
                                     -> [ExceptT String (ReaderT Bool m) ()]
getAndSaveVersionFilesFromLocalCache lCacheDir =
    map (getAndSaveVersionFileFromLocalCache lCacheDir)



getAndSaveVersionFileFromLocalCache :: MonadIO m
                                    => FilePath
                                    -> GitRepoNameAndVersion
                                    -> ExceptT String (ReaderT Bool m) ()
getAndSaveVersionFileFromLocalCache lCacheDir gitRepoNameAndVersion = do
  verbose <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  versionFileBinary <- getVersionFileFromLocalCache lCacheDir gitRepoNameAndVersion
  sayFunc $ "Found " <> versionFileName <> " in local cache at: " <> versionFileLocalCalchePath
  saveBinaryToFile versionFileBinary versionFileLocalPath
  sayFunc $ "Copied " <> versionFileName <> " to: " <> versionFileLocalPath

  where
   versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
   versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion
   versionFileLocalPath = carthageBuildDirectory </> versionFileName
   versionFileLocalCalchePath = lCacheDir </> versionFileRemotePath






-- | Downloads a list `FrameworkVersion` from which it derives dSYMs from a cache specified as `RomeCacheInfo`.
downloadFrameworksAndDsymsFromCaches :: S3.BucketName -- ^ The chache definition.
                                     -> Maybe FilePath
                                     -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                     -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to download.
                                     -> [TargetPlatform] -- ^ A list of target platforms restricting the scope of this action.
                                     -> ReaderT UDCEnv IO ()
downloadFrameworksAndDsymsFromCaches s3BucketName mlCacheDir reverseRomeMap fvs = mapM_ (sequence . downloadFramework)
  where
    downloadFramework = mapM (downloadFrameworkAndDsymFromCaches s3BucketName mlCacheDir reverseRomeMap) fvs



-- | Downloads a `FrameworkVersion` from which it derives dSYMs from a cache specified as `RomeCacheInfo`.
downloadFrameworkAndDsymFromCaches :: S3.BucketName -- ^ The chache definition.
                                   -> Maybe FilePath
                                   -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                   -> FrameworkVersion -- ^ The `FrameworkVersion` to download.
                                   -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                                   -> ReaderT UDCEnv IO ()
downloadFrameworkAndDsymFromCaches s3BucketName
                                   (Just lCacheDir)
                                   reverseRomeMap
                                   fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                                   platform = do
  (env, SkipLocalCacheFlag skipLocalCache, verbose) <- ask

  when skipLocalCache $
    downloadFrameworkAndDsymFromCaches s3BucketName Nothing reverseRomeMap fVersion platform

  unless skipLocalCache $ do

    eitherFrameworkSuccess <- runReaderT (runExceptT $ getAndUnzipFrameworkFromLocalCache lCacheDir reverseRomeMap fVersion platform) verbose
    case eitherFrameworkSuccess of
      Right _ -> return ()
      Left  e -> liftIO $ do
                  let sayFunc = if verbose then sayLnWithTime else sayLn
                  sayFunc e
                  runReaderT
                    ( do
                      let sayFunc2 = if verbose then sayLnWithTime else sayLn
                      e2 <- runExceptT $ do
                        frameworkBinary <- getFrameworkFromS3 s3BucketName reverseRomeMap fVersion platform
                        saveBinaryToLocalCache lCacheDir frameworkBinary remoteFrameworkUploadPath fwn verbose
                        unzipBinary frameworkBinary fwn frameworkZipName verbose <* makeExecutable platform f
                      whenError sayFunc2 e2
                     ) (env, verbose)


    eitherDSYMSuccess <- runReaderT (runExceptT $ getAndUnzipDSYMFromLocalCache lCacheDir reverseRomeMap fVersion platform) verbose
    case eitherDSYMSuccess of
     Right _ -> return ()
     Left  e -> liftIO $ do
                 let sayFunc = if verbose then sayLnWithTime else sayLn
                 sayFunc e
                 runReaderT
                   ( do
                     let sayFunc2 = if verbose then sayLnWithTime else sayLn
                     e2 <- runExceptT $ do
                       dSYMBinary <- getDSYMFromS3 s3BucketName reverseRomeMap fVersion platform
                       saveBinaryToLocalCache lCacheDir dSYMBinary remotedSYMUploadPath dSYMName verbose
                       unzipBinary dSYMBinary dSYMName dSYMZipName verbose
                     whenError sayFunc2 e2
                    ) (env, verbose)

  where
    frameworkZipName = frameworkArchiveName f version
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    dSYMZipName = dSYMArchiveName f version
    remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMName = fwn ++ ".dSYM"


downloadFrameworkAndDsymFromCaches s3BucketName
                                   Nothing
                                   reverseRomeMap
                                   frameworkVersion
                                   platform = do
  (env, _, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  eitherError <- liftIO $ runReaderT
                          (runExceptT $ getAndUnzipFrameworkFromS3 s3BucketName reverseRomeMap frameworkVersion platform)
                          (env, verbose)
  whenError sayFunc eitherError
  eitherDSYMError <- liftIO $ runReaderT
                     (runExceptT $ getAndUnzipDSYMFromS3 s3BucketName reverseRomeMap frameworkVersion platform)
                     (env, verbose)
  whenError sayFunc eitherDSYMError


whenError :: Monad m => (l -> m ()) -> Either l r -> m ()
whenError say (Left e)  = say e
whenError _ (Right _) = return ()



makeExecutable :: MonadIO m => TargetPlatform -> FrameworkName -> m Turtle.Permissions
makeExecutable p fname = Turtle.chmod Turtle.executable
                        (
                          Turtle.fromString $
                            frameworkBuildBundleForPlatform p fname
                            </> unFrameworkName fname
                        )



getFrameworkFromLocalCache :: MonadIO m
                           => FilePath
                           -> InvertedRepositoryMap
                           -> FrameworkVersion
                           -> TargetPlatform
                           -> ExceptT String m LBS.ByteString
getFrameworkFromLocalCache lCacheDir
                           reverseRomeMap
                           (FrameworkVersion f@(FrameworkName fwn) version)
                           platform = do
  frameworkExistsInLocalCache <- liftIO . doesFileExist $ frameworkLocalCachePath
  if frameworkExistsInLocalCache
    then liftIO . runResourceT $ C.sourceFile frameworkLocalCachePath C.$$ C.sinkLbs
    else throwError $ "Error: could not find " <> fwn <> " in local cache at : " <> frameworkLocalCachePath
  where
    frameworkLocalCachePath = lCacheDir </> remoteFrameworkUploadPath
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version


getDSYMFromLocalCache :: MonadIO m
                      => FilePath
                      -> InvertedRepositoryMap
                      -> FrameworkVersion
                      -> TargetPlatform
                      -> ExceptT String m LBS.ByteString
getDSYMFromLocalCache lCacheDir
                      reverseRomeMap
                      (FrameworkVersion f@(FrameworkName fwn) version)
                      platform = do
  dSYMExistsInLocalCache <- liftIO . doesFileExist $ dSYMLocalCachePath
  if dSYMExistsInLocalCache
    then liftIO . runResourceT $ C.sourceFile dSYMLocalCachePath C.$$ C.sinkLbs
    else throwError $ "Error: could not find " <> fwn <> " in local cache at : " <> dSYMLocalCachePath
  where
    dSYMLocalCachePath = lCacheDir </> remotedSYMUploadPath
    remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMName = fwn <> ".dSYM"

getVersionFileFromLocalCache :: MonadIO m
                             => FilePath
                             -> GitRepoNameAndVersion
                             -> ExceptT String m LBS.ByteString
getVersionFileFromLocalCache lCacheDir gitRepoNameAndVersion =  do
  versionFileExistsInLocalCache <- liftIO . doesFileExist $ versionFileLocalCalchePath

  if versionFileExistsInLocalCache
    then liftIO . runResourceT $ C.sourceFile versionFileLocalCalchePath C.$$ C.sinkLbs
    else throwError $ "Error: could not find " <> versionFileName <> " in local cache at : " <> versionFileLocalCalchePath
  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion
    versionFileLocalCalchePath = lCacheDir </> versionFileRemotePath



getAndUnzipFrameworksAndDSYMsFromLocalCache :: MonadIO m
                                            => FilePath
                                            -> InvertedRepositoryMap
                                            -> [FrameworkVersion]
                                            -> [TargetPlatform]
                                            -> [ExceptT String (ReaderT Bool m) ()]
getAndUnzipFrameworksAndDSYMsFromLocalCache lCacheDir
                                            reverseRomeMap
                                            fvs
                                            platforms =
  concatMap getAndUnzipFramework platforms <> concatMap getAndUnzipDSYM platforms
  where
  getAndUnzipFramework = mapM (getAndUnzipFrameworkFromLocalCache lCacheDir reverseRomeMap) fvs
  getAndUnzipDSYM      = mapM (getAndUnzipDSYMFromLocalCache lCacheDir reverseRomeMap) fvs



getAndUnzipFrameworkFromLocalCache :: MonadIO m
                                   => FilePath
                                   -> InvertedRepositoryMap
                                   -> FrameworkVersion
                                   -> TargetPlatform
                                   -> ExceptT String (ReaderT Bool m) ()
getAndUnzipFrameworkFromLocalCache lCacheDir
                                   reverseRomeMap
                                   fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                                   platform = do
  verbose <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  binary <- getFrameworkFromLocalCache lCacheDir reverseRomeMap fVersion platform
  sayFunc $ "Found " <> fwn <> " in local cache at: " <> frameworkLocalCachePath
  unzipBinary binary fwn frameworkZipName verbose <* makeExecutable platform f
  where
    frameworkLocalCachePath = lCacheDir </> remoteFrameworkUploadPath
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    frameworkZipName = frameworkArchiveName f version


getAndUnzipDSYMFromLocalCache :: MonadIO m
                              => FilePath
                              -> InvertedRepositoryMap
                              -> FrameworkVersion
                              -> TargetPlatform
                              -> ExceptT String (ReaderT Bool m) ()
getAndUnzipDSYMFromLocalCache lCacheDir
                              reverseRomeMap
                              fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                              platform = do
  verbose <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  binary <- getDSYMFromLocalCache lCacheDir reverseRomeMap fVersion platform
  sayFunc $ "Found " <> fwn <> " in local cache at: " <> dSYMLocalCachePath
  unzipBinary binary fwn dSYMZipName verbose <* makeExecutable platform f
  where
    dSYMLocalCachePath = lCacheDir </> remotedSYMUploadPath
    remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMZipName = dSYMArchiveName f version



getAndUnzipFrameworkFromS3 :: S3.BucketName
                           -> InvertedRepositoryMap
                           -> FrameworkVersion
                           -> TargetPlatform
                           -> ExceptT String (ReaderT (AWS.Env, Bool) IO) ()
getAndUnzipFrameworkFromS3 s3BucketName
                           reverseRomeMap
                           fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                           platform = do
    (_, verbose) <- ask
    frameworkBinary <- getFrameworkFromS3 s3BucketName reverseRomeMap fVersion platform
    unzipBinary frameworkBinary fwn frameworkZipName verbose
                             <* makeExecutable platform f
  where
    frameworkZipName = frameworkArchiveName f version



getFrameworkFromS3 :: S3.BucketName
                   -> InvertedRepositoryMap
                   -> FrameworkVersion
                   -> TargetPlatform
                   -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
getFrameworkFromS3 s3BucketName
                   reverseRomeMap
                   (FrameworkVersion f@(FrameworkName fwn) version)
                   platform = do
  getArtifactFromS3 s3BucketName remoteFrameworkUploadPath fwn
  where
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version



getAndUnzipDSYMFromS3 :: S3.BucketName
                      -> InvertedRepositoryMap
                      -> FrameworkVersion
                      -> TargetPlatform
                      -> ExceptT String (ReaderT (AWS.Env, Bool) IO) ()
getAndUnzipDSYMFromS3 s3BucketName
                      reverseRomeMap
                      fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                      platform = do
    (_, verbose) <- ask
    dSYMBinary <- getDSYMFromS3 s3BucketName reverseRomeMap fVersion platform
    unzipBinary dSYMBinary fwn dSYMZipName verbose
  where
      dSYMZipName = dSYMArchiveName f version



getDSYMFromS3 :: S3.BucketName
              -> InvertedRepositoryMap
              -> FrameworkVersion
              -> TargetPlatform
              -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
getDSYMFromS3 s3BucketName
              reverseRomeMap
              (FrameworkVersion f@(FrameworkName fwn) version)
              platform = do
  getArtifactFromS3 s3BucketName remoteDSYMUploadPath dSYMName
  where
    remoteDSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMName = fwn ++ ".dSYM"


getArtifactFromS3 :: S3.BucketName
                  -> FilePath
                  -> String
                  -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
getArtifactFromS3 s3BucketName
                  remotePath
                  name = do
  eitherArtifact <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotePath name
  case eitherArtifact of
    Left e -> throwError $ "Error: could not download " <> name <> " : " <> awsErrorToString e
    Right artifactBinary -> return artifactBinary

getVersionFileFromS3 :: S3.BucketName
                     -> GitRepoNameAndVersion
                     -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
getVersionFileFromS3 s3BucketName gitRepoNameAndVersion = do
  getArtifactFromS3 s3BucketName versionFileRemotePath versionFileName
  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion



-- | Downloads an artificat stored at a given path from an `S3.BucketName`.
downloadBinary s3BucketName objectRemotePath objectName = do
  (env, verbose) <- ask
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
probeS3ForFrameworks :: S3.BucketName -- ^ The chache definition.
                     -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                     -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
                     -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                     -> ReaderT (AWS.Env, Bool) IO [FrameworkAvailability]
probeS3ForFrameworks s3BucketName
                     reverseRomeMap
                     frameworkVersions = sequence . probeForEachFramework
  where
    probeForEachFramework = mapM (probeS3ForFramework s3BucketName reverseRomeMap) frameworkVersions



-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not in each `TargetPlatform`
probeS3ForFramework :: S3.BucketName -- ^ The chache definition.
                    -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                    -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                    -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                    -> ReaderT (AWS.Env, Bool) IO FrameworkAvailability
probeS3ForFramework s3BucketName
                    reverseRomeMap
                    frameworkVersion
                    platforms = fmap (FrameworkAvailability frameworkVersion) probeForEachPlatform
  where
    probeForEachPlatform = mapM (probeS3ForFrameworkOnPlatform s3BucketName reverseRomeMap frameworkVersion) platforms


-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not for a `TargetPlatform`.
probeS3ForFrameworkOnPlatform :: S3.BucketName -- ^ The chache definition.
                              -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                              -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                              -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                              -> ReaderT (AWS.Env, Bool) IO PlatformAvailability
probeS3ForFrameworkOnPlatform s3BucketName
                              reverseRomeMap
                              (FrameworkVersion fwn v)
                              platform = do
  (env, _) <- ask
  let isAvailable = runResourceT . AWS.runAWS env $ checkIfFrameworkExistsInBucket s3BucketName frameworkObjectKey
  PlatformAvailability platform <$> isAvailable
  where
    frameworkObjectKey = S3.ObjectKey . T.pack $ remoteFrameworkPath platform reverseRomeMap fwn v


probeLocalCacheForFrameworks :: MonadIO m
                             => FilePath -- ^ The chache definition.
                             -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                             -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
                             -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                             -> m [FrameworkAvailability]
probeLocalCacheForFrameworks lCacheDir
                             reverseRomeMap
                             frameworkVersions = sequence . probeForEachFramework
  where
    probeForEachFramework = mapM (probeLocalCacheForFramework lCacheDir reverseRomeMap) frameworkVersions



probeLocalCacheForFramework :: MonadIO m
                            => FilePath
                            -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                            -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                            -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                            -> m FrameworkAvailability
probeLocalCacheForFramework lCacheDir
                            reverseRomeMap
                            frameworkVersion
                            platforms = fmap (FrameworkAvailability frameworkVersion) probeForEachPlatform
  where
    probeForEachPlatform = mapM (probeLocalCacheForFrameworkOnPlatform lCacheDir reverseRomeMap frameworkVersion) platforms


probeLocalCacheForFrameworkOnPlatform :: MonadIO m
                                      => FilePath -- ^ The chache definition.
                                      -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                      -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                                      -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                                      -> m PlatformAvailability
probeLocalCacheForFrameworkOnPlatform lCacheDir
                                      reverseRomeMap
                                      (FrameworkVersion fwn version)
                                      platform = do
  frameworkExistsInLocalCache <- liftIO . doesFileExist $ frameworkLocalCachePath
  return (PlatformAvailability platform frameworkExistsInLocalCache)

  where
    frameworkLocalCachePath = lCacheDir </> remoteFrameworkUploadPath
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap fwn version





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

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


-- | Lists Frameworks in the caches.
listArtifacts :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
              -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
              -> ListMode -- ^ A list mode to execute this operation in.
              -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
              -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` from which to derive Frameworks
              -> [TargetPlatform] -- ^ A list of `TargetPlatform` to limit the operation to.
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



-- | Produces a list of `GitRepoAvailability`s for Frameworks
getRepoAvailabilityFromCaches :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
                              -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
                              -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                              -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` from which to derive Frameworks, dSYMs and .verison files
                              -> [TargetPlatform] -- ^ A list of `TargetPlatform`s to limit the operation to.
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




-- | Downloads Frameworks, related dSYMs and .version files in the caches.
downloadArtifacts :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
                  -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
                  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                  -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` from which to derive Frameworks, dSYMs and .verison files
                  -> [TargetPlatform] -- ^ A list of `TargetPlatform`s to limit the operation to.
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
            mapM_ (whenLeft sayFunc) errors
          ) verbose
        runReaderT
          (do
            let sayFunc = if verbose then sayLnWithTime else sayLn
            errors <- mapM runExceptT $ getAndSaveVersionFilesFromLocalCache lCacheDir gitRepoNamesAndVersions
            mapM_ (whenLeft sayFunc) errors
          ) verbose

    (Nothing, Nothing)  -> throwError bothCacheKeysMissingMessage

    where

      gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
      gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions



-- | Uploads Frameworks and relative dSYMs together with .version files to caches
uploadArtifacts :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
                -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
                -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` from which to derive Frameworks, dSYMs and .verison files
                -> [TargetPlatform] -- ^ A list of `TargetPlatform` to restrict this operation to.
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



-- | Saves a list of .version files to a local cache
saveVersionFilesToLocalCache :: FilePath -- ^ The cache definition.
                             -> [GitRepoNameAndVersion] -- ^ The information used to derive the name and path for the .version file.
                             -> ReaderT Bool IO ()
saveVersionFilesToLocalCache lCacheDir = mapM_ (saveVersonFileToLocalCache lCacheDir)



-- | Saves a .version file to a local Cache
saveVersonFileToLocalCache :: FilePath -- ^ The cache definition.
                           -> GitRepoNameAndVersion -- ^ The information used to derive the name and path for the .version file.
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



-- | Uploads a lest of .version files to the caches.
uploadVersionFilesToCaches :: S3.BucketName -- ^ The chache definition.
                           -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                           -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoName` and `Version` information.
                           -> ReaderT UDCEnv IO ()
uploadVersionFilesToCaches s3Bucket
                           mlCacheDir =
  mapM_ (uploadVersionFileToCaches s3Bucket mlCacheDir)



-- | Uploads a .version file the caches.
uploadVersionFileToCaches :: S3.BucketName -- ^ The chache definition.
                          -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                          -> GitRepoNameAndVersion -- ^ The information used to derive the name and path for the .version file.
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



-- | Uploads a .version file to an S3 Bucket
uploadVersionFileToS3 :: S3.BucketName -- ^ The cache definition.
                      -> LBS.ByteString -- ^ The contents of the .version file.
                      -> GitRepoNameAndVersion -- ^ The information used to derive the name and path for the .version file.
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



-- | Saves a `LBS.ByteString` representing a .version file to a file.
saveVersionFileBinaryToLocalCache :: MonadIO m
                                  => FilePath -- ^ The destinationf file.
                                  -> LBS.ByteString -- ^ The contents of the .version file
                                  -> GitRepoNameAndVersion  -- ^ The information used to derive the name and path for the .version file.
                                  -> Bool -- ^ A flag controlling verbosity.
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



-- | Uploads a list of Framewokrs and relative dSYMs to a caches.
uploadFrameworksAndDsymsToCaches :: S3.BucketName -- ^ The chache definition.
                                 -> Maybe FilePath -- ^ Just the path to a local cache or Nothing
                                 -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                 -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` idenfitying the Frameworks and dSYMs.
                                 -> [TargetPlatform] -- ^ A list of `TargetPlatform`s restricting the scope of this action.
                                 -> ReaderT UDCEnv IO ()
uploadFrameworksAndDsymsToCaches s3BucketName
                                 mlCacheDir
                                 reverseRomeMap
                                 fvs = mapM_ (sequence . upload)
  where
    upload = mapM (uploadFrameworkAndDsymToCaches s3BucketName mlCacheDir reverseRomeMap) fvs



-- | Uploads a Framework `Zip.Archive` to an S3 Bucket.
uploadFrameworkToS3 :: Zip.Archive -- ^ The `Zip.Archive` of the Framework.
                    -> S3.BucketName -- ^ The cache definition.
                    -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                    -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework.
                    -> TargetPlatform -- ^ A `TargetPlatform`s restricting the scope of this action.
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



-- | Uploads a dSYM `Zip.Archive` to an S3 Bucket.
uploadDsymToS3 :: Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
               -> S3.BucketName -- ^ The cache definition.
               -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
               -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM.
               -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
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



-- | Uploads a Framework and the relative dSYM to the caches.
uploadFrameworkAndDsymToCaches :: S3.BucketName -- ^ The chache definition.
                               -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                               -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                               -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM
                               -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
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


-- | Saves a list of Frameworks and relative dYSMs to a local cache.
saveFrameworksAndDSYMsToLocalCache :: MonadIO m
                                   => FilePath -- ^ The cache definition.
                                   -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                   -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` idenfitying Frameworks and dSYMs
                                   -> [TargetPlatform] -- ^ A list of `TargetPlatform` restricting the scope of this action.
                                   -> ReaderT Bool m ()
saveFrameworksAndDSYMsToLocalCache lCacheDir reverseRomeMap fvs = mapM_ (sequence . save)
  where
    save = mapM (saveFrameworkAndDSYMToLocalCache lCacheDir reverseRomeMap) fvs



-- | Saves a Framework the relative dYSM to a local cache.
saveFrameworkAndDSYMToLocalCache :: MonadIO m
                                 => FilePath -- ^ The cache definition
                                 -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                 -> FrameworkVersion -- ^ A `FrameworkVersion` idenfitying Framework and dSYM.
                                 -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
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




-- | Saves a Framework `Zip.Archive` to a local cache.
saveFrameworkToLocalCache :: FilePath -- ^ The cache definition.
                          -> Zip.Archive -- ^ The zipped archive of the Framework
                          -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                          -> FrameworkVersion -- ^ The `FrameworkVersion` indentifying the dSYM.
                          -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
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



-- | Saves a dSYM `Zip.Archive` to a local cache.
saveDsymToLocalCache :: FilePath -- ^ The cache definition.
                     -> Zip.Archive -- ^ The zipped archive of the dSYM.
                     -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                     -> FrameworkVersion -- ^ The `FrameworkVersion` indentifying the dSYM.
                     -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
                     -> ReaderT (SkipLocalCacheFlag, Bool) IO ()
saveDsymToLocalCache lCacheDir
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



-- | Creates a Zip archive of a file system directory
zipDir :: MonadIO m
       => FilePath -- ^ The directory to Zip.
       -> Bool -- ^ A flag controlling verbosity.
       -> ExceptT String m Zip.Archive
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



-- | Downloads a list of .version files from an S3 Bucket or a local cache.
downloadVersionFilesFromCaches :: S3.BucketName -- ^ The chache definition.
                               -> Maybe FilePath  -- ^ Just the local cache path or Nothing
                               -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoName`s and `Version`s information.
                               -> ReaderT UDCEnv IO ()
downloadVersionFilesFromCaches s3BucketName
                               lDir = mapM_ (downloadVersionFileFromCaches s3BucketName lDir)



-- | Downloads one .version file from an S3 Bucket or a local cache.
-- | If the .version file is not found in the local cache, it is downloaded from S3.
-- | If SkipLocalCache is specified, the local cache is ignored.
downloadVersionFileFromCaches :: S3.BucketName -- ^ The chache definition.
                              -> Maybe FilePath -- ^ Just the local cache path or Nothing
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
            whenLeft sayFunc2 e2
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
  whenLeft sayFunc eitherError
  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName



-- | Gets a multiple .version file from a local cache and saves them to the appropriate location.
getAndSaveVersionFilesFromLocalCache :: MonadIO m
                                     => FilePath -- ^ The cache definition.
                                     -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoNameAndVersion` identifying the .version files
                                     -> [ExceptT String (ReaderT Bool m) ()]
getAndSaveVersionFilesFromLocalCache lCacheDir =
    map (getAndSaveVersionFileFromLocalCache lCacheDir)



-- | Gets a .version file from a local cache and copies it to the approrpiate location.
getAndSaveVersionFileFromLocalCache :: MonadIO m
                                    => FilePath -- ^ The cache definition.
                                    -> GitRepoNameAndVersion -- ^ The `GitRepoNameAndVersion` identifying the .version file
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



-- | Downloads a list Frameworks and relative dSYMs from an S3 Bucket or a local cache.
downloadFrameworksAndDsymsFromCaches :: S3.BucketName -- ^ The chache definition.
                                     -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                                     -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                     -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` indentifying the Frameworks and dSYMs
                                     -> [TargetPlatform] -- ^ A list of target platforms restricting the scope of this action.
                                     -> ReaderT UDCEnv IO ()
downloadFrameworksAndDsymsFromCaches s3BucketName mlCacheDir reverseRomeMap fvs = mapM_ (sequence . downloadFramework)
  where
    downloadFramework = mapM (downloadFrameworkAndDsymFromCaches s3BucketName mlCacheDir reverseRomeMap) fvs



-- | Downloads a Framework and it's relative dSYM from and S3 Bucket or a local cache.
-- | If the Framework and dSYM are not found in the local cache then they are downloaded from S3.
-- | If SkipLocalCache is specified, the local cache is ignored.
downloadFrameworkAndDsymFromCaches :: S3.BucketName -- ^ The chache definition.
                                   -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                                   -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                   -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and dSYM
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
                      whenLeft sayFunc2 e2
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
                     whenLeft sayFunc2 e2
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
  whenLeft sayFunc eitherError
  eitherDSYMError <- liftIO $ runReaderT
                     (runExceptT $ getAndUnzipDSYMFromS3 s3BucketName reverseRomeMap frameworkVersion platform)
                     (env, verbose)
  whenLeft sayFunc eitherDSYMError


whenLeft :: Monad m => (l -> m ()) -> Either l r -> m ()
whenLeft f (Left e)  = f e
whenLeft _ (Right _) = return ()



-- | Adds executable permissions to a Framework. See https://github.com/blender/Rome/issues/57
makeExecutable :: MonadIO m
               => TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
               -> FrameworkName -- ^ The name of the Framework
               -> m Turtle.Permissions
makeExecutable p fname = Turtle.chmod Turtle.executable
                        (
                          Turtle.fromString $
                            frameworkBuildBundleForPlatform p fname
                            </> unFrameworkName fname
                        )




-- | Retrieves a Framework from a local cache
getFrameworkFromLocalCache :: MonadIO m
                           => FilePath -- ^ The cache definition
                           -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                           -> FrameworkVersion -- ^ The `FrameworkVersion` indentifying the Framework
                           -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
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



-- | Retrieves a dSYM from a local cache
getDSYMFromLocalCache :: MonadIO m
                      => FilePath -- ^ The cache definition
                      -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                      -> FrameworkVersion -- ^ The `FrameworkVersion` indentifying the dSYM
                      -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
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



-- | Retrieves a .version file from a local cache
getVersionFileFromLocalCache :: MonadIO m
                             => FilePath -- ^ The cache definition
                             -> GitRepoNameAndVersion -- ^ The `GitRepoNameAndVersion` used to indentify the .version file
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




-- | Retrieves a Frameworks and the corresponding dSYMs from a local cache for given `TargetPlatform`s, then unzips the contents
getAndUnzipFrameworksAndDSYMsFromLocalCache :: MonadIO m
                                            => FilePath -- ^ The cache definition
                                            -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                                            -> [FrameworkVersion] -- ^ The a list of `FrameworkVersion` identifying the Frameworks and dSYMs
                                            -> [TargetPlatform] -- ^ A list of `TargetPlatform`s to limit the operation to
                                            -> [ExceptT String (ReaderT Bool m) ()]
getAndUnzipFrameworksAndDSYMsFromLocalCache lCacheDir
                                            reverseRomeMap
                                            fvs
                                            platforms =
  concatMap getAndUnzipFramework platforms <> concatMap getAndUnzipDSYM platforms
  where
  getAndUnzipFramework = mapM (getAndUnzipFrameworkFromLocalCache lCacheDir reverseRomeMap) fvs
  getAndUnzipDSYM      = mapM (getAndUnzipDSYMFromLocalCache lCacheDir reverseRomeMap) fvs



-- | Retrieves a Framework from a local cache and unzip the contents
getAndUnzipFrameworkFromLocalCache :: MonadIO m
                                   => FilePath -- ^ The cache definition
                                   -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                                   -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                                   -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
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




-- | Retrieves a dSYM from a local cache yy and unzip the contents
getAndUnzipDSYMFromLocalCache :: MonadIO m
                              => FilePath -- ^ The cache definition
                              -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                              -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                              -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
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



-- | Retrieves a Framework from an S3 Cache and unzip the contents
getAndUnzipFrameworkFromS3 :: S3.BucketName -- ^ The cache definition
                           -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                           -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                           -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
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



-- | Retrieves a Framework from an S3 Cache and unzip the contents
getFrameworkFromS3 :: S3.BucketName -- ^ The cache definition
                   -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                   -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                   -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                   -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
getFrameworkFromS3 s3BucketName
                   reverseRomeMap
                   (FrameworkVersion f@(FrameworkName fwn) version)
                   platform = do
  getArtifactFromS3 s3BucketName remoteFrameworkUploadPath fwn
  where
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version



-- | Retrieves a dSYM from an S3 Cache and unzip the contents
getAndUnzipDSYMFromS3 :: S3.BucketName -- ^ The cache definition
                      -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                      -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
                      -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
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



-- | Retrieves a dSYM from an S3 Cache
getDSYMFromS3 :: S3.BucketName -- ^ The cache definition
              -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
              -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
              -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
              -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
getDSYMFromS3 s3BucketName
              reverseRomeMap
              (FrameworkVersion f@(FrameworkName fwn) version)
              platform = do
  getArtifactFromS3 s3BucketName remoteDSYMUploadPath dSYMName
  where
    remoteDSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMName = fwn ++ ".dSYM"



-- | Retrieves an artifact from an S3 Cache
getArtifactFromS3 :: S3.BucketName -- ^ The cache definition
                  -> FilePath -- ^ The path in the cache
                  -> String -- ^ A colloquial name for the artifact
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



-- | Probes a `FilePath` to check if each `FrameworkVersion` exists for each `TargetPlatform`
probeLocalCacheForFrameworks :: MonadIO m
                             => FilePath -- ^ The chache definition.
                             -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                             -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
                             -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                             -> m [FrameworkAvailability]
probeLocalCacheForFrameworks lCacheDir
                             reverseRomeMap
                             frameworkVersions =
  sequence . probeForEachFramework
  where
    probeForEachFramework = mapM (probeLocalCacheForFramework lCacheDir reverseRomeMap) frameworkVersions



-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for each `TargetPlatform`
probeLocalCacheForFramework :: MonadIO m
                            => FilePath -- ^ The chache definition.
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



-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for a given `TargetPlatform`
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
discoverRegion :: MonadIO m
               => ExceptT String m AWS.Region
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


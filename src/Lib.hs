{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}



{- Exports -}
module Lib (module Lib
           , Types.RomeVersion
           , Utils.romeVersionToString
           )
           where



{- Imports -}
import qualified Codec.Archive.Zip                    as Zip
import           Configuration
import           Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import           Control.Lens                         hiding (List)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader                 (ReaderT, ask, runReaderT,
                                                       withReaderT)
import           Control.Monad.Trans.Maybe            (exceptToMaybeT,
                                                       runMaybeT)
import           Control.Monad.Trans.Resource         (runResourceT)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import qualified Data.Conduit                         as C (Conduit, await,
                                                            yield, ($$), (=$=))
import qualified Data.Conduit.Binary                  as C (sinkFile, sinkLbs,
                                                            sourceFile,
                                                            sourceLbs)
import           Data.Either                          (lefts)
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid                          ((<>))
import           Data.Romefile
import qualified Data.S3Config                        as S3Config
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Network.AWS                          as AWS
import qualified Network.AWS.S3                       as S3
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO.Error                      (isDoesNotExistError)
import qualified Turtle
import           Types
import           Types.Commands                       as Commands
import           Utils
import           Xcode.DWARF


getAWSRegion :: (MonadIO m, MonadCatch m) => ExceptT String m AWS.Env
getAWSRegion = do
  region <- discoverRegion
  set AWS.envRegion region <$> AWS.newEnv AWS.Discover


bothCacheKeysMissingMessage :: String
bothCacheKeysMissingMessage = "Error: expected at least one of \"local\" or \
  \\"S3-Bucket\" key in the [Cache] section of your Romefile."

conflictingSkipLocalCacheOptionMessage :: String
conflictingSkipLocalCacheOptionMessage = "Error: only \"local\" key is present \
  \in the [Cache] section of your Romefile but you have asked Rome to skip \
  \this cache."

-- | Runs Rome with `RomeOptions` on a given a `AWS.Env`.
runRomeWithOptions :: RomeOptions -- ^ The `RomeOptions` to run Rome with.
                   -> RomeVersion
                   -> RomeMonad ()
runRomeWithOptions (RomeOptions options verbose) romeVersion = do
  cartfileEntries <- getCartfileEntires
  romeFileParseResult <- getRomefileEntries

  let respositoryMap = toRepositoryMap $ romeFileParseResult^.repositoryMapEntries
  let reverseRepositoryMap = toInvertedRepositoryMap $ romeFileParseResult^.repositoryMapEntries
  let ignoreNames = concatMap frameworkCommonNames $ romeFileParseResult^.ignoreMapEntries

  let cInfo = romeFileParseResult^.cacheInfo
  let mS3BucketName = S3.BucketName <$> cInfo^.bucket

  mlCacheDir <- liftIO $ traverse absolutizePath $ cInfo^.localCacheDir

  case options of

      Upload (RomeUDCPayload gitRepoNames platforms cachePrefixString skipLocalCache noIgnoreFlag) -> do

        sayVersionWarning romeVersion verbose

        let finalIgnoreNames = if _noIgnore noIgnoreFlag then [] else ignoreNames

        if null gitRepoNames
          then
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` finalIgnoreNames
                  cachePrefix = CachePrefix cachePrefixString in
              runReaderT
                (uploadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms)
                (cachePrefix, skipLocalCache, verbose)
          else
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) `filterOutFrameworkNamesAndVersionsIfNotIn` finalIgnoreNames
                  cachePrefix = CachePrefix cachePrefixString in
              runReaderT
                (uploadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms)
                (cachePrefix, skipLocalCache, verbose)

      Download (RomeUDCPayload gitRepoNames platforms cachePrefixString skipLocalCache noIgnoreFlag) -> do

        sayVersionWarning romeVersion verbose

        let finalIgnoreNames = if _noIgnore noIgnoreFlag then [] else ignoreNames

        if null gitRepoNames
          then
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` finalIgnoreNames
                  cachePrefix = CachePrefix cachePrefixString in
              runReaderT
                (downloadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms)
                (cachePrefix, skipLocalCache, verbose)
          else
              let frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap (filterCartfileEntriesByGitRepoNames gitRepoNames cartfileEntries) `filterOutFrameworkNamesAndVersionsIfNotIn` finalIgnoreNames
                  cachePrefix = CachePrefix cachePrefixString in
              runReaderT
                (downloadArtifacts mS3BucketName mlCacheDir reverseRepositoryMap frameworkVersions platforms)
                (cachePrefix, skipLocalCache, verbose)

      List (RomeListPayload listMode platforms cachePrefixString printFormat noIgnoreFlag) ->
          let finalIgnoreNames = if _noIgnore noIgnoreFlag then [] else ignoreNames
              frameworkVersions = deriveFrameworkNamesAndVersion respositoryMap cartfileEntries `filterOutFrameworkNamesAndVersionsIfNotIn` finalIgnoreNames
              cachePrefix = CachePrefix cachePrefixString in
          runReaderT
            (listArtifacts mS3BucketName mlCacheDir listMode reverseRepositoryMap frameworkVersions platforms printFormat)
            (cachePrefix, SkipLocalCacheFlag False, verbose)

  where
    sayVersionWarning vers verb = runMaybeT $ exceptToMaybeT $ do
              let sayFunc = if verb then sayLnWithTime else sayLn
              (uptoDate, latestVersion) <- checkIfRomeLatestVersionIs vers
              unless uptoDate $ sayFunc $ redControlSequence
                <> "*** Please update to the latest Rome version: "
                <> romeVersionToString latestVersion
                <> ". "
                <> "You are currently on: "
                <> romeVersionToString vers
                <> noColorControlSequence


-- | Lists Frameworks in the caches.
listArtifacts :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
              -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
              -> ListMode -- ^ A list mode to execute this operation in.
              -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
              -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` from which to derive Frameworks
              -> [TargetPlatform] -- ^ A list of `TargetPlatform` to limit the operation to.
              -> PrintFormat -- ^ A format of the string result: text or JSON.
              -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) RomeMonad ()
listArtifacts mS3BucketName
              mlCacheDir
              listMode
              reverseRepositoryMap
              frameworkVersions
              platforms
              format = do
  (_, _, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  repoAvailabilities <- getRepoAvailabilityFromCaches mS3BucketName
                                                      mlCacheDir
                                                      reverseRepositoryMap
                                                      frameworkVersions platforms
  if format == Text
    then mapM_ sayFunc $ repoLines repoAvailabilities
    else sayFunc $ toJSONStr $ ReposJSON (fmap formattedRepoAvailabilityJSON repoAvailabilities)
  where
    repoLines repoAvailabilities = filter (not . null) $
                                      fmap (formattedRepoAvailability listMode)
                                           repoAvailabilities



-- | Produces a list of `GitRepoAvailability`s for Frameworks
getRepoAvailabilityFromCaches :: Maybe S3.BucketName -- ^ Just an S3 Bucket name or Nothing
                              -> Maybe FilePath -- ^ Just the path to the local cache or Nothing
                              -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                              -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` from which to derive Frameworks, dSYMs and .verison files
                              -> [TargetPlatform] -- ^ A list of `TargetPlatform`s to limit the operation to.
                              -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) RomeMonad [GitRepoAvailability]
getRepoAvailabilityFromCaches (Just s3BucketName)
                              _
                              reverseRepositoryMap
                              frameworkVersions
                              platforms = do
  env <- lift getAWSRegion
  (cachePrefix, _, verbose) <- ask
  let readerEnv = (env, cachePrefix, verbose)
  availabilities <- liftIO $ runReaderT (probeS3ForFrameworks s3BucketName reverseRepositoryMap frameworkVersions platforms) readerEnv
  return $ getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRepositoryMap availabilities

getRepoAvailabilityFromCaches Nothing
                              (Just lCacheDir)
                              reverseRepositoryMap
                              frameworkVersions
                              platforms = do
  (cachePrefix, SkipLocalCacheFlag skipLocalCache, _) <- ask
  when skipLocalCache $
    throwError conflictingSkipLocalCacheOptionMessage

  availabilities <- probeLocalCacheForFrameworks lCacheDir cachePrefix reverseRepositoryMap frameworkVersions platforms
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
                  -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) RomeMonad ()
downloadArtifacts mS3BucketName
                  mlCacheDir
                  reverseRepositoryMap
                  frameworkVersions
                  platforms = do
  (cachePrefix, s@(SkipLocalCacheFlag skipLocalCache), verbose) <- ask

  let
    sayFunc :: MonadIO m => String -> m ()
    sayFunc = if verbose then sayLnWithTime else sayLn

  case (mS3BucketName, mlCacheDir) of

    (Just s3BucketName,  lCacheDir) -> do
      env <- lift getAWSRegion
      let uploadDownloadEnv = (env, cachePrefix, s, verbose)
      liftIO $ runReaderT
        (downloadFrameworksAndArtifactsFromCaches s3BucketName lCacheDir reverseRepositoryMap frameworkVersions platforms)
        uploadDownloadEnv
      liftIO $ runReaderT
        (downloadVersionFilesFromCaches s3BucketName lCacheDir gitRepoNamesAndVersions)
        uploadDownloadEnv

    (Nothing, Just lCacheDir) -> do

      let readerEnv = (cachePrefix, verbose)
      when skipLocalCache $
        throwError conflictingSkipLocalCacheOptionMessage

      liftIO $ do
        runReaderT
          (do
            errors <- mapM runExceptT $
              getAndUnzipFrameworksAndArtifactsFromLocalCache lCacheDir reverseRepositoryMap frameworkVersions platforms
            mapM_ (whenLeft sayFunc) errors
          ) readerEnv
        runReaderT
          (do
            errors <- mapM runExceptT $
              getAndSaveVersionFilesFromLocalCache lCacheDir gitRepoNamesAndVersions
            mapM_ (whenLeft sayFunc) errors
          ) readerEnv

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
                -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) RomeMonad ()
uploadArtifacts mS3BucketName
                mlCacheDir
                reverseRepositoryMap
                frameworkVersions
                platforms = do
  (cachePrefix, s@(SkipLocalCacheFlag skipLocalCache), verbose) <- ask
  case (mS3BucketName, mlCacheDir) of
    (Just s3BucketName,  lCacheDir) -> do
      env <- lift getAWSRegion
      let uploadDownloadEnv = (env, cachePrefix, s, verbose)
      liftIO $ runReaderT
        (uploadFrameworksAndArtifactsToCaches s3BucketName lCacheDir reverseRepositoryMap frameworkVersions platforms)
        uploadDownloadEnv
      liftIO $ runReaderT
        (uploadVersionFilesToCaches s3BucketName lCacheDir gitRepoNamesAndVersions)
        uploadDownloadEnv

    (Nothing, Just lCacheDir) -> do
      let readerEnv = (cachePrefix, verbose)
      when skipLocalCache $
        throwError conflictingSkipLocalCacheOptionMessage
      liftIO $
        runReaderT (saveFrameworksAndArtifactsToLocalCache lCacheDir reverseRepositoryMap frameworkVersions platforms) readerEnv
        >> runReaderT (saveVersionFilesToLocalCache lCacheDir gitRepoNamesAndVersions) readerEnv


    (Nothing, Nothing)  -> throwError bothCacheKeysMissingMessage

    where

      gitRepoNamesAndVersions :: [GitRepoNameAndVersion]
      gitRepoNamesAndVersions = repoNamesAndVersionForFrameworkVersions reverseRepositoryMap frameworkVersions



-- | Saves a list of .version files to a local cache
saveVersionFilesToLocalCache :: FilePath -- ^ The cache definition.
                             -> [GitRepoNameAndVersion] -- ^ The information used to derive the name and path for the .version file.
                             -> ReaderT (CachePrefix, Bool) IO ()
saveVersionFilesToLocalCache lCacheDir = mapM_ (saveVersonFileToLocalCache lCacheDir)



-- | Saves a .version file to a local Cache
saveVersonFileToLocalCache :: FilePath -- ^ The cache definition.
                           -> GitRepoNameAndVersion -- ^ The information used to derive the name and path for the .version file.
                           -> ReaderT (CachePrefix, Bool) IO ()
saveVersonFileToLocalCache lCacheDir
                           gitRepoNameAndVersion = do
  (cachePrefix, verbose) <- ask
  versionFileExists <- liftIO $ doesFileExist versionFileLocalPath

  when versionFileExists $ do
    versionFileContent <- liftIO $ LBS.readFile versionFileLocalPath
    saveVersionFileBinaryToLocalCache lCacheDir cachePrefix versionFileContent gitRepoNameAndVersion verbose

  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName



-- | Uploads a lest of .version files to the caches.
uploadVersionFilesToCaches :: S3.BucketName -- ^ The chache definition.
                           -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                           -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoName` and `Version` information.
                           -> ReaderT UploadDownloadCmdEnv IO ()
uploadVersionFilesToCaches s3Bucket
                           mlCacheDir =
  mapM_ (uploadVersionFileToCaches s3Bucket mlCacheDir)



-- | Uploads a .version file the caches.
uploadVersionFileToCaches :: S3.BucketName -- ^ The chache definition.
                          -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                          -> GitRepoNameAndVersion -- ^ The information used to derive the name and path for the .version file.
                          -> ReaderT UploadDownloadCmdEnv IO ()
uploadVersionFileToCaches s3BucketName
                          mlCacheDir
                          gitRepoNameAndVersion = do
  (env, cachePrefix, SkipLocalCacheFlag skipLocalCache, verbose) <- ask

  versionFileExists <- liftIO $ doesFileExist versionFileLocalPath

  when versionFileExists $ do
     versionFileContent <- liftIO $ LBS.readFile versionFileLocalPath
     unless skipLocalCache $
      maybe (return ()) liftIO $
          saveVersionFileBinaryToLocalCache
            <$> mlCacheDir
            <*> Just cachePrefix
            <*> Just versionFileContent
            <*> Just gitRepoNameAndVersion
            <*> Just verbose
     liftIO $ runReaderT (uploadVersionFileToS3 s3BucketName versionFileContent gitRepoNameAndVersion) (env, cachePrefix, verbose)

  where

    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName



-- | Uploads a .version file to an S3 Bucket
uploadVersionFileToS3 :: S3.BucketName -- ^ The cache definition.
                      -> LBS.ByteString -- ^ The contents of the .version file.
                      -> GitRepoNameAndVersion -- ^ The information used to derive the name and path for the .version file.
                      -> ReaderT (AWS.Env, CachePrefix, Bool) IO ()
uploadVersionFileToS3  s3BucketName
                       versionFileContent
                       gitRepoNameAndVersion = do
  (env, CachePrefix prefix, verbose) <- ask
  withReaderT (const (env, verbose)) $
    uploadBinary s3BucketName
                 versionFileContent
                 (prefix </>versionFileRemotePath)
                 versionFileName

  where

    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion



-- | Saves a `LBS.ByteString` representing a .version file to a file.
saveVersionFileBinaryToLocalCache :: MonadIO m
                                  => FilePath -- ^ The destinationf file.
                                  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
                                  -> LBS.ByteString -- ^ The contents of the .version file
                                  -> GitRepoNameAndVersion  -- ^ The information used to derive the name and path for the .version file.
                                  -> Bool -- ^ A flag controlling verbosity.
                                  -> m ()
saveVersionFileBinaryToLocalCache lCacheDir
                                  (CachePrefix prefix)
                                  versionFileContent
                                  gitRepoNameAndVersion =
  saveBinaryToLocalCache lCacheDir
                         versionFileContent
                         (prefix </> versionFileRemotePath)
                         versionFileName

  where

    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion



-- | Uploads a list of Framewokrs and relative dSYMs to a caches.
uploadFrameworksAndArtifactsToCaches :: S3.BucketName -- ^ The chache definition.
                                     -> Maybe FilePath -- ^ Just the path to a local cache or Nothing
                                     -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                     -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` idenfitying the Frameworks and dSYMs.
                                     -> [TargetPlatform] -- ^ A list of `TargetPlatform`s restricting the scope of this action.
                                     -> ReaderT UploadDownloadCmdEnv IO ()
uploadFrameworksAndArtifactsToCaches s3BucketName
                                     mlCacheDir
                                     reverseRomeMap
                                     fvs = mapM_ (sequence . upload)
  where
    upload = mapM (uploadFrameworkAndArtifactsToCaches s3BucketName mlCacheDir reverseRomeMap) fvs



-- | Uploads a Framework `Zip.Archive` to an S3 Bucket.
uploadFrameworkToS3 :: Zip.Archive -- ^ The `Zip.Archive` of the Framework.
                    -> S3.BucketName -- ^ The cache definition.
                    -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                    -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework.
                    -> TargetPlatform -- ^ A `TargetPlatform`s restricting the scope of this action.
                    -> ReaderT UploadDownloadEnv IO ()
uploadFrameworkToS3 frameworkArchive
                    s3BucketName
                    reverseRomeMap
                    (FrameworkVersion f@(FrameworkName fwn) version)
                    platform = do
  (env, CachePrefix prefix, verbose) <- ask
  withReaderT (const (env, verbose)) $
    uploadBinary s3BucketName (Zip.fromArchive frameworkArchive) (prefix </> remoteFrameworkUploadPath) fwn

  where
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version



-- | Uploads a dSYM `Zip.Archive` to an S3 Bucket.
uploadDsymToS3 :: Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
               -> S3.BucketName -- ^ The cache definition.
               -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
               -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM.
               -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
               -> ReaderT UploadDownloadEnv IO ()
uploadDsymToS3 dSYMArchive
               s3BucketName
               reverseRomeMap
               (FrameworkVersion f@(FrameworkName fwn) version)
               platform = do
  (env, CachePrefix prefix, verbose) <- ask
  withReaderT (const (env, verbose)) $
    uploadBinary s3BucketName (Zip.fromArchive dSYMArchive) (prefix </> remoteDsymUploadPath) (fwn <> ".dSYM")

  where
    remoteDsymUploadPath = remoteDsymPath platform reverseRomeMap f version

-- | Uploads a bcsymbolmap `Zip.Archive` to an S3 Bucket.
uploadBcsymbolmapToS3 :: DwarfUUID -- ^ The UUID of the bcsymblmap
                      -> Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
                      -> S3.BucketName -- ^ The cache definition.
                      -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                      -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM.
                      -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
                      -> ReaderT UploadDownloadEnv IO ()
uploadBcsymbolmapToS3 dwarfUUID
               dwarfArchive
               s3BucketName
               reverseRomeMap
               (FrameworkVersion f@(FrameworkName fwn) version)
               platform = do
  (env, CachePrefix prefix, verbose) <- ask
  withReaderT (const (env, verbose)) $
    uploadBinary s3BucketName
                (Zip.fromArchive dwarfArchive)
                (prefix </> remoteBcsymbolmapUploadPath)
                (fwn <> "." <> bcsymbolmapNameFrom dwarfUUID)

  where
    remoteBcsymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version



-- | Uploads a Framework, the relative dSYM and bcsybolmaps to the caches.
uploadFrameworkAndArtifactsToCaches :: S3.BucketName -- ^ The chache definition.
                                    -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                                    -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                    -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM
                                    -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
                                    -> ReaderT UploadDownloadCmdEnv IO ()
uploadFrameworkAndArtifactsToCaches s3BucketName
                               mlCacheDir
                               reverseRomeMap
                               fVersion@(FrameworkVersion f@(FrameworkName fwn) _)
                               platform = do
  (env,  cachePrefix, s@(SkipLocalCacheFlag skipLocalCache), verbose) <- ask

  let uploadDownloadEnv = (env, cachePrefix, verbose)

  void . runExceptT $ do
    frameworkArchive <- createZipArchive frameworkDirectory verbose
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
          <*> Just (cachePrefix, s, verbose)
    liftIO $ runReaderT
      (uploadFrameworkToS3 frameworkArchive s3BucketName reverseRomeMap fVersion platform)
      uploadDownloadEnv

  void . runExceptT $ do
    dSYMArchive <- createZipArchive dSYMdirectory verbose
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
          <*> Just (cachePrefix, s, verbose)
    liftIO $ runReaderT
      (uploadDsymToS3 dSYMArchive s3BucketName reverseRomeMap fVersion platform)
      uploadDownloadEnv

  void . runExceptT $ do
    dwarfUUIDs <- dwarfUUIDsFrom (frameworkDirectory </> fwn)
    maybeUUIDsArchives <- liftIO $ forM dwarfUUIDs $ \dwarfUUID -> runMaybeT $ do
                        dwarfArchive <- exceptToMaybeT $ createZipArchive (bcSybolMapPath dwarfUUID) verbose
                        return (dwarfUUID, dwarfArchive)

    unless skipLocalCache $
        forM_ maybeUUIDsArchives $ mapM $
          \(dwarfUUID, dwarfArchive) -> maybe (return ()) liftIO $
             runReaderT
               <$> (
                   saveBcsymbolmapToLocalCache
                   <$> mlCacheDir
                   <*> Just dwarfUUID
                   <*> Just dwarfArchive
                   <*> Just reverseRomeMap
                   <*> Just fVersion
                   <*> Just platform
                   )
               <*> Just (cachePrefix, s, verbose)

    forM_ maybeUUIDsArchives $ mapM $
      \(dwarfUUID, dwarfArchive) -> liftIO $
        runReaderT
          (uploadBcsymbolmapToS3 dwarfUUID dwarfArchive s3BucketName reverseRomeMap fVersion platform)
          uploadDownloadEnv

  where

    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension
    dSYMNameWithDSYMExtension = frameworkNameWithFrameworkExtension <> ".dSYM"
    dSYMdirectory = platformBuildDirectory </> dSYMNameWithDSYMExtension
    bcSybolMapPath d = platformBuildDirectory </> bcsymbolmapNameFrom d


-- | Saves a list of Frameworks and relative dYSMs to a local cache.
saveFrameworksAndArtifactsToLocalCache :: MonadIO m
                                       => FilePath -- ^ The cache definition.
                                       -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                       -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` idenfitying Frameworks and dSYMs
                                       -> [TargetPlatform] -- ^ A list of `TargetPlatform` restricting the scope of this action.
                                       -> ReaderT (CachePrefix, Bool) m ()
saveFrameworksAndArtifactsToLocalCache lCacheDir reverseRomeMap fvs = mapM_ (sequence . save)
  where
    save = mapM (saveFrameworkAndArtifactsToLocalCache lCacheDir reverseRomeMap) fvs



-- | Saves a Framework, the relative dYSM and then bcsymbolmaps to a local cache.
saveFrameworkAndArtifactsToLocalCache :: MonadIO m
                                      => FilePath -- ^ The cache definition
                                      -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                      -> FrameworkVersion -- ^ A `FrameworkVersion` idenfitying Framework and dSYM.
                                      -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
                                      -> ReaderT (CachePrefix, Bool) m ()
saveFrameworkAndArtifactsToLocalCache lCacheDir
                                      reverseRomeMap
                                      fVersion@(FrameworkVersion f@(FrameworkName fwn) _)
                                      platform = do
  (cachePrefix, verbose) <- ask
  let readerEnv = (cachePrefix, SkipLocalCacheFlag False, verbose)

  void . runExceptT $ do
    frameworkArchive <- createZipArchive frameworkDirectory verbose
    liftIO $
      runReaderT
        (saveFrameworkToLocalCache lCacheDir frameworkArchive reverseRomeMap fVersion platform)
        readerEnv

  void . runExceptT $ do
    dSYMArchive <- createZipArchive dSYMdirectory verbose
    liftIO $
      runReaderT
        (saveDsymToLocalCache lCacheDir dSYMArchive reverseRomeMap fVersion platform)
        readerEnv

  void . runExceptT $ do
    dwarfUUIDs <- dwarfUUIDsFrom (frameworkDirectory </> fwn)
    maybeUUIDsArchives <- liftIO $ forM dwarfUUIDs $ \dwarfUUID -> runMaybeT $ do
                        dwarfArchive <- exceptToMaybeT $ createZipArchive (bcSybolMapPath dwarfUUID) verbose
                        return (dwarfUUID, dwarfArchive)
    forM_ maybeUUIDsArchives $ mapM $
      \(dwarfUUID, dwarfArchive) -> liftIO $
         runReaderT
          (saveBcsymbolmapToLocalCache lCacheDir dwarfUUID dwarfArchive reverseRomeMap fVersion platform)
          readerEnv

  where
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension
    dSYMNameWithDSYMExtension = frameworkNameWithFrameworkExtension <> ".dSYM"
    dSYMdirectory = platformBuildDirectory </> dSYMNameWithDSYMExtension
    bcSybolMapPath d = platformBuildDirectory </> bcsymbolmapNameFrom d





-- | Saves a Framework `Zip.Archive` to a local cache.
saveFrameworkToLocalCache :: FilePath -- ^ The cache definition.
                          -> Zip.Archive -- ^ The zipped archive of the Framework
                          -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                          -> FrameworkVersion -- ^ The `FrameworkVersion` indentifying the dSYM.
                          -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
                          -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) IO ()
saveFrameworkToLocalCache lCacheDir
                          frameworkArchive
                          reverseRomeMap
                          (FrameworkVersion f@(FrameworkName _) version)
                          platform = do
  (CachePrefix prefix, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  unless skipLocalCache $
   saveBinaryToLocalCache lCacheDir
                          (Zip.fromArchive frameworkArchive)
                          (prefix </> remoteFrameworkUploadPath)
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
                     -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) IO ()
saveDsymToLocalCache lCacheDir
                     dSYMArchive
                     reverseRomeMap
                     (FrameworkVersion f@(FrameworkName fwn) version)
                     platform = do
  (CachePrefix prefix, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  unless skipLocalCache $
   saveBinaryToLocalCache lCacheDir
                          (Zip.fromArchive dSYMArchive)
                          (prefix </> remoteDsymUploadPath)
                          (fwn <> ".dSYM")
                          verbose
  where
    remoteDsymUploadPath = remoteDsymPath platform reverseRomeMap f version


-- | Saves a bcsymbolmap `Zip.Archive` to a local cache.
saveBcsymbolmapToLocalCache :: FilePath -- ^ The cache definition.
                            -> DwarfUUID -- ^ The UUID of the bcsymblmap
                            -> Zip.Archive -- ^ The zipped archive of the bcsymbolmap.
                            -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                            -> FrameworkVersion -- ^ The `FrameworkVersion` indentifying the dSYM.
                            -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
                            -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) IO ()
saveBcsymbolmapToLocalCache lCacheDir
                     dwarfUUID
                     dwarfArchive
                     reverseRomeMap
                     (FrameworkVersion f@(FrameworkName _) version)
                     platform = do
  (CachePrefix prefix, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
  unless skipLocalCache $
    saveBinaryToLocalCache lCacheDir
                          (Zip.fromArchive dwarfArchive)
                          (prefix </> remoteBcSymbolmapUploadPath)
                          (bcsymbolmapNameFrom dwarfUUID)
                          verbose
  where
    remoteBcSymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID
                                                        platform
                                                        reverseRomeMap
                                                        f
                                                        version



-- | Uploads an artificat to an `S3.BucketName` at a given path in the bucket.
uploadBinary :: AWS.ToBody a
             => S3.BucketName
             -> a
             -> FilePath
             -> FilePath
             -> ReaderT (AWS.Env, Bool) IO ()
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
                       -> FilePath -- ^ The destination path inside the base directory.
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
                               -> ReaderT UploadDownloadCmdEnv IO ()
downloadVersionFilesFromCaches s3BucketName
                               lDir = mapM_ (downloadVersionFileFromCaches s3BucketName lDir)



-- | Downloads one .version file from an S3 Bucket or a local cache.
-- | If the .version file is not found in the local cache, it is downloaded from S3.
-- | If SkipLocalCache is specified, the local cache is ignored.
downloadVersionFileFromCaches :: S3.BucketName -- ^ The chache definition.
                              -> Maybe FilePath -- ^ Just the local cache path or Nothing
                              -> GitRepoNameAndVersion -- ^ The `GitRepoName` and `Version` information.
                              -> ReaderT UploadDownloadCmdEnv IO ()
downloadVersionFileFromCaches s3BucketName (Just lCacheDir) gitRepoNameAndVersion = do
  (env, cachePrefix@(CachePrefix prefix), SkipLocalCacheFlag skipLocalCache, verbose) <- ask

  when skipLocalCache $
    downloadVersionFileFromCaches s3BucketName Nothing gitRepoNameAndVersion

  unless skipLocalCache $ do
    eitherSuccess <- runReaderT (runExceptT $ getAndSaveVersionFileFromLocalCache lCacheDir gitRepoNameAndVersion)
                                (cachePrefix, verbose)
    case eitherSuccess of
      Right _ -> return ()
      Left e -> liftIO $ do
        let
          sayFunc :: MonadIO m => String -> m ()
          sayFunc = if verbose then sayLnWithTime else sayLn
        sayFunc e
        runReaderT
          (do
            e2 <- runExceptT $ do
              versionFileBinary <- getVersionFileFromS3 s3BucketName gitRepoNameAndVersion
              saveBinaryToLocalCache lCacheDir versionFileBinary (prefix </> versionFileRemotePath) versionFileName verbose
              saveBinaryToFile versionFileBinary versionFileLocalPath
              sayFunc $ "Copied " <> versionFileName <> " to: " <> versionFileLocalPath
            whenLeft sayFunc e2
           ) (env, cachePrefix, verbose)

  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion

downloadVersionFileFromCaches s3BucketName Nothing gitRepoNameAndVersion = do
  (env, cachePrefix, _, verbose) <- ask
  let
    sayFunc :: MonadIO m => String -> m ()
    sayFunc = if verbose then sayLnWithTime else sayLn
  eitherError <- liftIO $ runReaderT
                          (runExceptT $ do
                            versionFileBinary <- getVersionFileFromS3 s3BucketName gitRepoNameAndVersion
                            saveBinaryToFile versionFileBinary versionFileLocalPath
                            sayFunc $ "Copied " <> versionFileName <> " to: " <> versionFileLocalPath
                          )
                          (env, cachePrefix, verbose)
  whenLeft sayFunc eitherError
  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileLocalPath = carthageBuildDirectory </> versionFileName



-- | Gets a multiple .version file from a local cache and saves them to the appropriate location.
getAndSaveVersionFilesFromLocalCache :: MonadIO m
                                     => FilePath -- ^ The cache definition.
                                     -> [GitRepoNameAndVersion] -- ^ A list of `GitRepoNameAndVersion` identifying the .version files
                                     -> [ExceptT String (ReaderT (CachePrefix, Bool) m) ()]
getAndSaveVersionFilesFromLocalCache lCacheDir =
    map (getAndSaveVersionFileFromLocalCache lCacheDir)



-- | Gets a .version file from a local cache and copies it to the approrpiate location.
getAndSaveVersionFileFromLocalCache :: MonadIO m
                                    => FilePath -- ^ The cache definition.
                                    -> GitRepoNameAndVersion -- ^ The `GitRepoNameAndVersion` identifying the .version file
                                    -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndSaveVersionFileFromLocalCache lCacheDir gitRepoNameAndVersion = do
  (cachePrefix@(CachePrefix prefix), verbose) <- ask
  let finalVersionFileLocalCachePath = versionFileLocalCachePath prefix
  let sayFunc = if verbose then sayLnWithTime else sayLn
  versionFileBinary <- getVersionFileFromLocalCache lCacheDir cachePrefix gitRepoNameAndVersion
  sayFunc $ "Found " <> versionFileName <> " in local cache at: " <> finalVersionFileLocalCachePath
  saveBinaryToFile versionFileBinary versionFileLocalPath
  sayFunc $ "Copied " <> versionFileName <> " to: " <> versionFileLocalPath

  where
   versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
   versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion
   versionFileLocalPath = carthageBuildDirectory </> versionFileName
   versionFileLocalCachePath cPrefix = lCacheDir </> cPrefix </> versionFileRemotePath



-- | Downloads a list Frameworks and relative dSYMs from an S3 Bucket or a local cache.
downloadFrameworksAndArtifactsFromCaches :: S3.BucketName -- ^ The chache definition.
                                         -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                                         -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                         -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` indentifying the Frameworks and dSYMs
                                         -> [TargetPlatform] -- ^ A list of target platforms restricting the scope of this action.
                                         -> ReaderT UploadDownloadCmdEnv IO ()
downloadFrameworksAndArtifactsFromCaches s3BucketName mlCacheDir reverseRomeMap fvs = mapM_ (sequence . downloadFramework)
  where
    downloadFramework = mapM (downloadFrameworkAndArtifactsFromCaches s3BucketName mlCacheDir reverseRomeMap) fvs



-- | Downloads a Framework and it's relative dSYM from and S3 Bucket or a local cache.
-- | If the Framework and dSYM are not found in the local cache then they are downloaded from S3.
-- | If SkipLocalCache is specified, th local cache is ignored.
downloadFrameworkAndArtifactsFromCaches :: S3.BucketName -- ^ The chache definition.
                                        -> Maybe FilePath -- ^ Just the path to the local cache or Nothing.
                                        -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                        -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and dSYM
                                        -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                                        -> ReaderT UploadDownloadCmdEnv IO ()
downloadFrameworkAndArtifactsFromCaches s3BucketName
                                        (Just lCacheDir)
                                        reverseRomeMap
                                        fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                                        platform = do
  (env, cachePrefix@(CachePrefix prefix), SkipLocalCacheFlag skipLocalCache, verbose) <- ask

  let remoteReaderEnv = (env, cachePrefix, verbose)
  let localReaderEnv  = (cachePrefix, verbose)

  when skipLocalCache $
    downloadFrameworkAndArtifactsFromCaches s3BucketName Nothing reverseRomeMap fVersion platform

  unless skipLocalCache $ do
    eitherFrameworkSuccess <- runReaderT (runExceptT $ getAndUnzipFrameworkFromLocalCache lCacheDir reverseRomeMap fVersion platform)
                                         localReaderEnv
    let
      sayFunc :: MonadIO m => String -> m ()
      sayFunc = if verbose then sayLnWithTime else sayLn

    case eitherFrameworkSuccess of
      Right _ -> return ()
      Left  e -> liftIO $ do
                  sayFunc e
                  runReaderT
                    ( do
                      e2 <- runExceptT $ do
                        frameworkBinary <- getFrameworkFromS3 s3BucketName reverseRomeMap fVersion platform
                        saveBinaryToLocalCache lCacheDir frameworkBinary (prefix </> remoteFrameworkUploadPath) fwn verbose
                        deleteFrameworkDirectory fVersion platform verbose
                        unzipBinary frameworkBinary fwn frameworkZipName verbose <* makeExecutable platform f
                      whenLeft sayFunc e2
                     ) remoteReaderEnv


    eitherBcsymbolmapsOrErrors <- runReaderT (runExceptT $ getAndUnzipBcsymbolmapsFromLocalCache' lCacheDir reverseRomeMap fVersion platform)
                                             localReaderEnv
    case eitherBcsymbolmapsOrErrors of
      Right _ ->
          return ()
      Left ErrorGettingDwarfUUIDs ->
        sayFunc $ "Error: Cannot retrieve symbolmaps ids for " <> fwn
      Left (FailedDwarfUUIDs dwardUUIDsAndErrors) -> do
        mapM_ (sayFunc . snd) dwardUUIDsAndErrors
        forM_ (map fst dwardUUIDsAndErrors) $ \dwarfUUID ->
          liftIO $ runReaderT
            ( do
              e <- runExceptT $ do
                  let symbolmapLoggingName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID
                  let bcsymbolmapZipName d = bcsymbolmapArchiveName d version
                  let localBcsybolmapPathFrom d = platformBuildDirectory </> bcsymbolmapNameFrom d
                  symbolmapBinary <- getBcsymbolmapFromS3 s3BucketName reverseRomeMap fVersion platform dwarfUUID
                  saveBinaryToLocalCache lCacheDir symbolmapBinary (prefix </> remoteBcSymbolmapUploadPathFromDwarf dwarfUUID) fwn verbose
                  deleteFile (localBcsybolmapPathFrom dwarfUUID) verbose
                  unzipBinary symbolmapBinary symbolmapLoggingName (bcsymbolmapZipName dwarfUUID) verbose
              whenLeft sayFunc e
            ) remoteReaderEnv


    eitherDSYMSuccess <- runReaderT (runExceptT $ getAndUnzipDSYMFromLocalCache lCacheDir reverseRomeMap fVersion platform)
                                    localReaderEnv
    case eitherDSYMSuccess of
     Right _ -> return ()
     Left  e -> liftIO $ do
                 sayFunc e
                 runReaderT
                   ( do
                     e2 <- runExceptT $ do
                       dSYMBinary <- getDSYMFromS3 s3BucketName reverseRomeMap fVersion platform
                       saveBinaryToLocalCache lCacheDir dSYMBinary (prefix </> remotedSYMUploadPath) dSYMName verbose
                       deleteDSYMDirectory fVersion platform verbose
                       unzipBinary dSYMBinary dSYMName dSYMZipName verbose
                     whenLeft sayFunc e2
                    ) remoteReaderEnv

  where
    frameworkZipName = frameworkArchiveName f version
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    remoteBcSymbolmapUploadPathFromDwarf dwarfUUID = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version
    dSYMZipName = dSYMArchiveName f version
    remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    dSYMName = fwn <> ".dSYM"


downloadFrameworkAndArtifactsFromCaches s3BucketName
                                        Nothing
                                        reverseRomeMap
                                        fVersion@(FrameworkVersion (FrameworkName fwn) _)
                                        platform = do
  (env, cachePrefix,  _, verbose) <- ask

  let readerEnv = (env, cachePrefix, verbose)

  let sayFunc = if verbose then sayLnWithTime else sayLn
  eitherError <- liftIO $ runReaderT
                          (runExceptT $ getAndUnzipFrameworkFromS3 s3BucketName reverseRomeMap fVersion platform)
                          readerEnv
  whenLeft sayFunc eitherError

  eitherDSYMError <- liftIO $ runReaderT
                     (runExceptT $ getAndUnzipDSYMFromS3 s3BucketName reverseRomeMap fVersion platform)
                     readerEnv
  whenLeft sayFunc eitherDSYMError

  eitherSymbolmapsOrErrors <- liftIO $ runReaderT
                     (runExceptT $ getAndUnzipBcsymbolmapsFromS3' s3BucketName reverseRomeMap fVersion platform)
                     readerEnv
  flip whenLeft eitherSymbolmapsOrErrors $ \e ->
    case e of
      ErrorGettingDwarfUUIDs ->
        sayFunc $ "Error: Cannot retrieve symbolmaps ids for " <> fwn
      (FailedDwarfUUIDs dwardUUIDsAndErrors) ->
          mapM_ (sayFunc . snd) dwardUUIDsAndErrors

  -- whenLeft sayFunc eitherDSYMError


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



-- | Delete a directory an all it's contents
deleteDirectory :: MonadIO m
                => FilePath -- ^ The path to the directory to delete
                -> Bool -- ^ A flag controlling verbosity
                -> m ()
deleteDirectory path
                verbose = do
  directoryExists <- liftIO $ doesDirectoryExist path
  let sayFunc = if verbose then sayLnWithTime else sayLn
  when directoryExists $ do
    Turtle.rmtree . Turtle.fromString $ path
    when verbose $
      sayFunc $ "Deleted: " <> path

-- | Delete a file
deleteFile :: MonadIO m
           => FilePath -- ^ The path to the directory to delete
           -> Bool -- ^ A flag controlling verbosity
           -> m ()
deleteFile path
          verbose = do
  let sayFunc = if verbose then sayLnWithTime else sayLn
  liftIO $ removeFile path `catch` handleError sayFunc
  when verbose $
      liftIO . sayFunc $ "Deleted: " <> path
  where
    handleError f e
      | isDoesNotExistError e = f $ "Error: no such file " <> path
      | otherwise = throwM e



-- | Deletes a Framework from the Carthage Build folder
deleteFrameworkDirectory :: MonadIO m
                         => FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework to delete
                         -> TargetPlatform -- ^ The `TargetPlatform` to restrict this operation to
                         -> Bool -- ^ A flag controlling verbosity
                         -> m ()
deleteFrameworkDirectory (FrameworkVersion f _)
                         platform =
  deleteDirectory frameworkDirectory
  where
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension



-- | Deletes a dSYM from the Carthage Build folder
deleteDSYMDirectory :: MonadIO m
                    => FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM to delete
                    -> TargetPlatform -- ^ The `TargetPlatform` to restrict this operation to
                    -> Bool -- ^ A flag controlling verbosity
                    -> m ()
deleteDSYMDirectory (FrameworkVersion f _)
                    platform =
  deleteDirectory dSYMDirectory
  where
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    dSYMDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension <> ".dSYM"



-- | Retrieves a Framework from a local cache
getFrameworkFromLocalCache :: MonadIO m
                           => FilePath -- ^ The cache definition
                           -> CachePrefix -- ^ A prefix for folders at top level in the cache.
                           -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                           -> FrameworkVersion -- ^ The `FrameworkVersion` indentifying the Framework
                           -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                           -> ExceptT String m LBS.ByteString
getFrameworkFromLocalCache lCacheDir
                           (CachePrefix prefix)
                           reverseRomeMap
                           (FrameworkVersion f@(FrameworkName fwn) version)
                           platform = do
  frameworkExistsInLocalCache <- liftIO . doesFileExist $ frameworkLocalCachePath prefix
  if frameworkExistsInLocalCache
    then liftIO . runResourceT $ C.sourceFile (frameworkLocalCachePath prefix) C.$$ C.sinkLbs
    else throwError $ "Error: could not find " <> fwn <> " in local cache at : " <> frameworkLocalCachePath prefix
  where
    frameworkLocalCachePath cPrefix = lCacheDir </> cPrefix </> remoteFrameworkUploadPath
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version



-- | Retrieves a dSYM from a local cache
getDSYMFromLocalCache :: MonadIO m
                      => FilePath -- ^ The cache definition
                      -> CachePrefix -- ^ A prefix for folders at top level in the cache.
                      -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                      -> FrameworkVersion -- ^ The `FrameworkVersion` indentifying the dSYM
                      -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                      -> ExceptT String m LBS.ByteString
getDSYMFromLocalCache lCacheDir
                      (CachePrefix prefix)
                      reverseRomeMap
                      (FrameworkVersion f@(FrameworkName fwn) version)
                      platform = do
  let finalDSYMLocalPath = dSYMLocalCachePath prefix
  dSYMExistsInLocalCache <- liftIO . doesFileExist $ finalDSYMLocalPath
  if dSYMExistsInLocalCache
    then liftIO . runResourceT $ C.sourceFile finalDSYMLocalPath C.$$ C.sinkLbs
    else throwError $ "Error: could not find " <> dSYMName <> " in local cache at : " <> finalDSYMLocalPath
  where
    dSYMLocalCachePath cPrefix = lCacheDir </> cPrefix </> remotedSYMUploadPath
    remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMName = fwn <> ".dSYM"


-- | Retrieves a bcsymbolmap from a local cache
getBcsymbolmapFromLocalCache :: MonadIO m
                             => FilePath -- ^ The cache definition
                             -> CachePrefix -- ^ A prefix for folders at top level in the cache.
                             -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                             -> FrameworkVersion -- ^ The `FrameworkVersion` indentifying the dSYM
                             -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                             -> DwarfUUID -- ^ The UUID of the bcsymbolmap
                             -> ExceptT String m LBS.ByteString
getBcsymbolmapFromLocalCache lCacheDir
                             (CachePrefix prefix)
                             reverseRomeMap
                             (FrameworkVersion f@(FrameworkName fwn) version)
                             platform
                             dwarfUUID = do
  let finalBcsymbolmapLocalPath = bcsymbolmapLocalCachePath prefix
  bcSymbolmapExistsInLocalCache <- liftIO . doesFileExist $ finalBcsymbolmapLocalPath
  if bcSymbolmapExistsInLocalCache
    then liftIO . runResourceT $ C.sourceFile finalBcsymbolmapLocalPath C.$$ C.sinkLbs
    else throwError $ "Error: could not find " <> bcsymbolmapName <> " in local cache at : " <> finalBcsymbolmapLocalPath
  where
    remoteBcsymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version
    bcsymbolmapLocalCachePath cPrefix = lCacheDir </> cPrefix </> remoteBcsymbolmapUploadPath
    bcsymbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID




-- | Retrieves a .version file from a local cache
getVersionFileFromLocalCache :: MonadIO m
                             => FilePath -- ^ The cache definition
                             -> CachePrefix -- ^ A prefix for folders at top level in the cache.
                             -> GitRepoNameAndVersion -- ^ The `GitRepoNameAndVersion` used to indentify the .version file
                             -> ExceptT String m LBS.ByteString
getVersionFileFromLocalCache lCacheDir
                             (CachePrefix prefix)
                             gitRepoNameAndVersion = do
  versionFileExistsInLocalCache <- liftIO . doesFileExist $ versionFileLocalCachePath

  if versionFileExistsInLocalCache
    then liftIO . runResourceT $ C.sourceFile versionFileLocalCachePath C.$$ C.sinkLbs
    else throwError $ "Error: could not find " <> versionFileName <> " in local cache at : " <> versionFileLocalCachePath
  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion
    versionFileLocalCachePath = lCacheDir </> prefix </>versionFileRemotePath




-- | Retrieves a Frameworks and the corresponding dSYMs from a local cache for given `TargetPlatform`s, then unzips the contents
getAndUnzipFrameworksAndArtifactsFromLocalCache :: MonadIO m
                                                => FilePath -- ^ The cache definition
                                                -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                                                -> [FrameworkVersion] -- ^ The a list of `FrameworkVersion` identifying the Frameworks and dSYMs
                                                -> [TargetPlatform] -- ^ A list of `TargetPlatform`s to limit the operation to
                                                -> [ExceptT String (ReaderT (CachePrefix, Bool) m) ()]
getAndUnzipFrameworksAndArtifactsFromLocalCache lCacheDir
                                                reverseRomeMap
                                                fvs
                                                platforms =
  concatMap getAndUnzipFramework platforms
    <> concatMap getAndUnzipBcsymbolmaps platforms
    <> concatMap getAndUnzipDSYM platforms
  where
  getAndUnzipFramework    = mapM (getAndUnzipFrameworkFromLocalCache lCacheDir reverseRomeMap) fvs
  getAndUnzipBcsymbolmaps = mapM (getAndUnzipBcsymbolmapsFromLocalCache lCacheDir reverseRomeMap) fvs
  getAndUnzipDSYM         = mapM (getAndUnzipDSYMFromLocalCache lCacheDir reverseRomeMap) fvs



-- | Retrieves a Framework from a local cache and unzip the contents
getAndUnzipFrameworkFromLocalCache :: MonadIO m
                                   => FilePath -- ^ The cache definition
                                   -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                                   -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                                   -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                                   -> ExceptT String (ReaderT (CachePrefix , Bool) m) ()
getAndUnzipFrameworkFromLocalCache lCacheDir
                                   reverseRomeMap
                                   fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                                   platform = do
  (cachePrefix@(CachePrefix prefix), verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  binary <- getFrameworkFromLocalCache lCacheDir cachePrefix reverseRomeMap fVersion platform
  sayFunc $ "Found " <> fwn <> " in local cache at: " <> frameworkLocalCachePath prefix
  deleteFrameworkDirectory fVersion platform verbose
  unzipBinary binary fwn frameworkZipName verbose <* makeExecutable platform f
  where
    frameworkLocalCachePath cPrefix = lCacheDir </> cPrefix </> remoteFrameworkUploadPath
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    frameworkZipName = frameworkArchiveName f version




-- | Retrieves a dSYM from a local cache yy and unzip the contents
getAndUnzipDSYMFromLocalCache :: MonadIO m
                              => FilePath -- ^ The cache definition
                              -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                              -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                              -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                              -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipDSYMFromLocalCache lCacheDir
                              reverseRomeMap
                              fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                              platform = do
  (cachePrefix@(CachePrefix prefix), verbose) <- ask
  let finalDSYMLocalPath = dSYMLocalCachePath prefix
  let sayFunc = if verbose then sayLnWithTime else sayLn
  binary <- getDSYMFromLocalCache lCacheDir cachePrefix reverseRomeMap fVersion platform
  sayFunc $ "Found " <> dSYMName <> " in local cache at: " <> finalDSYMLocalPath
  deleteDSYMDirectory fVersion platform verbose
  unzipBinary binary fwn dSYMZipName verbose <* makeExecutable platform f
  where
    dSYMLocalCachePath cPrefix = lCacheDir </> cPrefix </> remotedSYMUploadPath
    remotedSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMZipName = dSYMArchiveName f version
    dSYMName = fwn <> ".dSYM"



-- | Retrieves all the bcsymbolmap files from a local cache and unzip the contents
getAndUnzipBcsymbolmapsFromLocalCache :: MonadIO m
                                      => FilePath -- ^ The cache definition
                                      -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                                      -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                                      -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                                      -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipBcsymbolmapsFromLocalCache lCacheDir
                                      reverseRomeMap
                                      fVersion@(FrameworkVersion f@(FrameworkName fwn) _)
                                      platform = do
  (_, verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn

  dwarfUUIDs <- dwarfUUIDsFrom (frameworkDirectory </> fwn)
  mapM_ (\dwarfUUID ->
    getAndUnzipBcSymbolmapFromLocalCache lCacheDir reverseRomeMap fVersion platform dwarfUUID `catchError` sayFunc)
    dwarfUUIDs
  where
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension


data DWARFOpeartionError = ErrorGettingDwarfUUIDs
                         | FailedDwarfUUIDs [(DwarfUUID, String)]


-- | Retrieves all the bcsymbolmap files from a local cache and unzip the contents
getAndUnzipBcsymbolmapsFromLocalCache' :: MonadIO m
                                       => FilePath -- ^ The cache definition
                                       -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                                       -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                                       -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                                       -> ExceptT DWARFOpeartionError (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipBcsymbolmapsFromLocalCache' lCacheDir
                                       reverseRomeMap
                                       fVersion@(FrameworkVersion f@(FrameworkName fwn) _)
                                       platform = do

  dwarfUUIDs <- withExceptT (const ErrorGettingDwarfUUIDs) $ dwarfUUIDsFrom (frameworkDirectory </> fwn)
  eitherDwarfUUIDsOrSucces <- forM dwarfUUIDs
    (\dwarfUUID ->
      lift $ runExceptT (withExceptT
        (\e -> (dwarfUUID, e)) $
          getAndUnzipBcSymbolmapFromLocalCache lCacheDir reverseRomeMap fVersion platform dwarfUUID))

  let failedUUIDsAndErrors = lefts eitherDwarfUUIDsOrSucces
  unless (null failedUUIDsAndErrors) $
      throwError $ FailedDwarfUUIDs failedUUIDsAndErrors

  where
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension



getAndUnzipBcSymbolmapFromLocalCache :: MonadIO m
                                     => FilePath -- ^ The cache definition
                                     -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                                     -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                                     -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                                     -> DwarfUUID
                                     -> ExceptT String (ReaderT (CachePrefix, Bool) m) ()
getAndUnzipBcSymbolmapFromLocalCache lCacheDir
                                     reverseRomeMap
                                     fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                                     platform
                                     dwarfUUID = do
  (cachePrefix@(CachePrefix prefix), verbose) <- ask
  let sayFunc = if verbose then sayLnWithTime else sayLn
  let symbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID
  binary <- getBcsymbolmapFromLocalCache lCacheDir cachePrefix reverseRomeMap fVersion platform dwarfUUID
  sayFunc $ "Found " <> symbolmapName <> " in local cache at: " <> frameworkLocalCachePath prefix
  deleteFile (bcsybolmapPath dwarfUUID) verbose
  unzipBinary binary symbolmapName (bcsymbolmapZipName dwarfUUID) verbose
  where
    frameworkLocalCachePath cPrefix = lCacheDir </> cPrefix </> remoteFrameworkUploadPath
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version
    bcsymbolmapZipName d = bcsymbolmapArchiveName d version
    bcsybolmapPath d = platformBuildDirectory </> bcsymbolmapNameFrom d
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform




-- | Retrieves a Framework from an S3 Cache and unzip the contents
getAndUnzipFrameworkFromS3 :: S3.BucketName -- ^ The cache definition
                           -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                           -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                           -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                           -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipFrameworkFromS3 s3BucketName
                           reverseRomeMap
                           fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                           platform = do
    (_, _, verbose) <- ask
    frameworkBinary <- getFrameworkFromS3 s3BucketName reverseRomeMap fVersion platform
    deleteFrameworkDirectory fVersion platform verbose
    unzipBinary frameworkBinary fwn frameworkZipName verbose
                             <* makeExecutable platform f
  where
    frameworkZipName = frameworkArchiveName f version



-- | Retrieves a Framework from an S3 Cache and unzip the contents
getFrameworkFromS3 :: S3.BucketName -- ^ The cache definition
                   -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
                   -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                   -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                   -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
getFrameworkFromS3 s3BucketName
                   reverseRomeMap
                   (FrameworkVersion f@(FrameworkName fwn) version)
                   platform = do
  (env, CachePrefix prefix, verbose) <- ask
  mapExceptT (withReaderT (const (env, verbose)))
             (getArtifactFromS3 s3BucketName (prefix </> remoteFrameworkUploadPath) fwn)
  where
    remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version



-- | Retrieves a dSYM from an S3 Cache and unzip the contents
getAndUnzipDSYMFromS3 :: S3.BucketName -- ^ The cache definition
                      -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                      -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
                      -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                      -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipDSYMFromS3 s3BucketName
                      reverseRomeMap
                      fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
                      platform = do
    (_, _, verbose) <- ask
    dSYMBinary <- getDSYMFromS3 s3BucketName reverseRomeMap fVersion platform
    deleteDSYMDirectory fVersion platform verbose
    unzipBinary dSYMBinary fwn dSYMZipName verbose
  where
      dSYMZipName = dSYMArchiveName f version

-- | Retrieves a dSYM from an S3 Cache and unzip the contents
getAndUnzipBcsymbolmapFromS3 :: S3.BucketName -- ^ The cache definition
                             -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                             -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
                             -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                             -> DwarfUUID -- ^ The UUID of the bcsymblmap
                             -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipBcsymbolmapFromS3 s3BucketName
                             reverseRomeMap
                             fVersion@(FrameworkVersion (FrameworkName fwn) version)
                             platform
                             dwarfUUID = do
    (_, _, verbose) <- ask
    let symbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID
    binary <- getBcsymbolmapFromS3 s3BucketName reverseRomeMap fVersion platform dwarfUUID
    deleteFile (bcsybolmapPath dwarfUUID) verbose
    unzipBinary binary symbolmapName (bcsymbolmapZipName dwarfUUID) verbose
  where
      platformBuildDirectory = carthageBuildDirectoryForPlatform platform
      bcsymbolmapZipName d = bcsymbolmapArchiveName d version
      bcsybolmapPath d = platformBuildDirectory </> bcsymbolmapNameFrom d


-- | Retrieves all the bcsymbolmap files from S3 and unzip the contents
getAndUnzipBcsymbolmapsFromS3' :: S3.BucketName -- ^ The cache definition
                               -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                               -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
                               -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                               -> ExceptT DWARFOpeartionError (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipBcsymbolmapsFromS3' lCacheDir
                               reverseRomeMap
                               fVersion@(FrameworkVersion f@(FrameworkName fwn) _)
                               platform = do

  dwarfUUIDs <- withExceptT (const ErrorGettingDwarfUUIDs) $ dwarfUUIDsFrom (frameworkDirectory </> fwn)
  eitherDwarfUUIDsOrSucces <- forM dwarfUUIDs
    (\dwarfUUID ->
      lift $ runExceptT (withExceptT
        (\e -> (dwarfUUID, e)) $
          getBcsymbolmapFromS3 lCacheDir reverseRomeMap fVersion platform dwarfUUID))

  let failedUUIDsAndErrors = lefts eitherDwarfUUIDsOrSucces
  unless (null failedUUIDsAndErrors) $
      throwError $ FailedDwarfUUIDs failedUUIDsAndErrors

  where
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension


-- | Retrieves a dSYM from an S3 Cache
getDSYMFromS3 :: S3.BucketName -- ^ The cache definition
              -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
              -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
              -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
              -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
getDSYMFromS3 s3BucketName
              reverseRomeMap
              (FrameworkVersion f@(FrameworkName fwn) version)
              platform = do
  (env, CachePrefix prefix, verbose) <-  ask
  let finalRemoteDSYMUploadPath = prefix </> remoteDSYMUploadPath
  mapExceptT (withReaderT (const (env, verbose))) $
              getArtifactFromS3 s3BucketName finalRemoteDSYMUploadPath dSYMName
  where
    remoteDSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
    dSYMName = fwn <> ".dSYM"


-- | Retrieves a bcsymbolmap from an S3 Cache
getBcsymbolmapFromS3 :: S3.BucketName -- ^ The cache definition
                     -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
                     -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
                     -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
                     -> DwarfUUID -- ^ The UUID of the bcsymblmap
                     -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
getBcsymbolmapFromS3 s3BucketName
                     reverseRomeMap
                     (FrameworkVersion f@(FrameworkName fwn) version)
                     platform
                     dwarfUUID = do
  (env, CachePrefix prefix, verbose) <-  ask
  let finalRemoteBcsymbolmaploadPath = prefix </> remoteBcSymbolmapUploadPath
  mapExceptT (withReaderT (const (env, verbose))) $
              getArtifactFromS3 s3BucketName finalRemoteBcsymbolmaploadPath symbolmapName
  where
    remoteBcSymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version
    symbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID


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

-- | Retrieves a .version file from S3
getVersionFileFromS3 :: S3.BucketName
                     -> GitRepoNameAndVersion
                     -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
getVersionFileFromS3 s3BucketName
                     gitRepoNameAndVersion = do
  (env, CachePrefix prefix, verbose) <- ask
  let finalVersionFileRemotePath = prefix </> versionFileRemotePath
  mapExceptT (withReaderT (const (env, verbose))) $
    getArtifactFromS3 s3BucketName finalVersionFileRemotePath versionFileName
  where
    versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
    versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion



-- | Downloads an artificat stored at a given path from an `S3.BucketName`.
downloadBinary :: S3.BucketName
               -> FilePath
               -> FilePath
               -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
downloadBinary s3BucketName objectRemotePath objectName = do
  (env, verbose) <- ask
  runResourceT . AWS.runAWS env $ do
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose $
      sayFunc $ "Started downloading " <> objectName <> " from: " <> objectRemotePath
    rs <- AWS.send $ S3.getObject s3BucketName objectKey
    let contentLength = fromIntegral $ fromMaybe 0 $ view S3.gorsContentLength rs
    binary <- view S3.gorsBody rs `AWS.sinkBody` sink verbose contentLength
    sayFunc $ "Downloaded " <> objectName <> " from: " <> objectRemotePath
    return binary

  where
    objectKey = S3.ObjectKey . T.pack $ objectRemotePath
    sink verbose totalLength = if verbose then printProgress objectName totalLength C.=$= C.sinkLbs else C.sinkLbs

    printProgress :: MonadIO m => String -> Int -> C.Conduit BS.ByteString m BS.ByteString
    printProgress objName totalLength = loop totalLength 0 0
      where
        loop t consumedLen lastLen = C.await >>= maybe (return ()) (\bs -> do
            let len = consumedLen + BS.length bs
            let diffGreaterThan1MB = len - lastLen >= 1024*1024
            when ( diffGreaterThan1MB || len == t) $
               sayLnWithTime $ "Downloaded " <> showInMegabytes len <> " of " <> showInMegabytes totalLength <> " for " <> objName
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
  liftIO $ Zip.extractFilesFromArchive [Zip.OptRecursive, Zip.OptPreserveSymbolicLinks] (Zip.toArchive objectBinary)
  when verbose $
    sayLnWithTime $ "Unzipped " <> objectName <> " from: " <> objectZipName



-- | Probes the caches described by `RomeCacheInfo` to check whether a list of `FrameworkVersion` is present or not
-- | in the caches for each `TargetPlatform`
probeS3ForFrameworks :: S3.BucketName -- ^ The chache definition.
                     -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                     -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
                     -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                     -> ReaderT (AWS.Env, CachePrefix, Bool) IO [FrameworkAvailability]
probeS3ForFrameworks s3BucketName
                     reverseRomeMap
                     frameworkVersions
                     platforms = mapConcurrently probe frameworkVersions
  where
    probe framework = probeS3ForFramework s3BucketName reverseRomeMap framework platforms



-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not in each `TargetPlatform`
probeS3ForFramework :: S3.BucketName -- ^ The chache definition.
                    -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                    -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                    -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                    -> ReaderT (AWS.Env, CachePrefix, Bool) IO FrameworkAvailability
probeS3ForFramework s3BucketName
                    reverseRomeMap
                    frameworkVersion
                    platforms = fmap (FrameworkAvailability frameworkVersion) probeForEachPlatform
  where
    probeForEachPlatform = mapConcurrently (probeS3ForFrameworkOnPlatform s3BucketName reverseRomeMap frameworkVersion) platforms


-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not for a `TargetPlatform`.
probeS3ForFrameworkOnPlatform :: S3.BucketName -- ^ The chache definition.
                              -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                              -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                              -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                              -> ReaderT (AWS.Env, CachePrefix, Bool) IO PlatformAvailability
probeS3ForFrameworkOnPlatform s3BucketName
                              reverseRomeMap
                              (FrameworkVersion fwn v)
                              platform = do
  (env, CachePrefix prefixStr, _) <- ask
  let isAvailable = runResourceT . AWS.runAWS env $ checkIfFrameworkExistsInBucket s3BucketName (frameworkObjectKeyWithPrefix prefixStr)
  PlatformAvailability platform <$> isAvailable
  where
    frameworkObjectKeyWithPrefix cPrefix = S3.ObjectKey . T.pack $ cPrefix </> remoteFrameworkPath platform reverseRomeMap fwn v



-- | Probes a `FilePath` to check if each `FrameworkVersion` exists for each `TargetPlatform`
probeLocalCacheForFrameworks :: MonadIO m
                             => FilePath -- ^ The chache definition.
                             -> CachePrefix -- ^ A prefix for folders at top level in the cache.
                             -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                             -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
                             -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                             -> m [FrameworkAvailability]
probeLocalCacheForFrameworks lCacheDir
                             cachePrefix
                             reverseRomeMap
                             frameworkVersions =
  sequence . probeForEachFramework
  where
    probeForEachFramework = mapM (probeLocalCacheForFramework lCacheDir cachePrefix reverseRomeMap) frameworkVersions



-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for each `TargetPlatform`
probeLocalCacheForFramework :: MonadIO m
                            => FilePath -- ^ The chache definition.
                            -> CachePrefix -- ^ A prefix for folders at top level in the cache.
                            -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                            -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                            -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
                            -> m FrameworkAvailability
probeLocalCacheForFramework lCacheDir
                            cachePrefix
                            reverseRomeMap
                            frameworkVersion
                            platforms = fmap (FrameworkAvailability frameworkVersion) probeForEachPlatform
  where
    probeForEachPlatform = mapM (probeLocalCacheForFrameworkOnPlatform lCacheDir cachePrefix reverseRomeMap frameworkVersion) platforms



-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for a given `TargetPlatform`
probeLocalCacheForFrameworkOnPlatform :: MonadIO m
                                      => FilePath -- ^ The chache definition.
                                      -> CachePrefix -- ^ A prefix for folders at top level in the cache.
                                      -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
                                      -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
                                      -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
                                      -> m PlatformAvailability
probeLocalCacheForFrameworkOnPlatform lCacheDir
                                      (CachePrefix prefix)
                                      reverseRomeMap
                                      (FrameworkVersion fwn version)
                                      platform = do
  frameworkExistsInLocalCache <- liftIO . doesFileExist $ frameworkLocalCachePath
  return (PlatformAvailability platform frameworkExistsInLocalCache)

  where
    frameworkLocalCachePath = lCacheDir </> prefix </> remoteFrameworkUploadPath
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


formattedRepoAvailabilityJSON :: GitRepoAvailability
                              -> RepoJSON
formattedRepoAvailabilityJSON (GitRepoAvailability (GitRepoName name) (Version version) ps) =
  RepoJSON { name = name, Types.version = version, present = getPlatforms Commands.Present, missing = getPlatforms Commands.Missing }
  where
    getPlatforms mode = show . _availabilityPlatform <$> filterAccordingToListMode mode ps


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
  file <- liftIO (T.readFile f)
  withExceptT (("Could not parse " <> f <> ": ") <>) . ExceptT . return $ do
    config <- S3Config.parseS3Config file
    S3Config.regionOf (T.pack profile) config

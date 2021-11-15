module Caches.Local.Uploading where



import qualified Codec.Archive.Zip             as Zip
import           Configuration
import           Control.Monad                            ( unless
                                                          , when
                                                          )
import           Control.Monad.IO.Class
import           Control.Monad.Reader                     ( ReaderT
                                                          , ask
                                                          )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Carthage.TargetPlatform
import           Data.Monoid                              ( (<>) )
import           Data.Romefile                            ( Framework(..) )
import           System.Directory
import           System.FilePath                          ( (</>) )
import           Types                             hiding ( version )
import           Types.Commands                           ( SkipLocalCacheFlag(..) )
import           Utils
import           Xcode.DWARF



-- | Saves a Framework `Zip.Archive` to a local cache.
saveFrameworkToLocalCache
  :: FilePath -- ^ The cache definition.
  -> Zip.Archive -- ^ The zipped archive of the Framework
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
  -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) IO ()
saveFrameworkToLocalCache lCacheDir frameworkArchive useXcFrameworks reverseRomeMap (FrameworkVersion f@(Framework _ _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (CachePrefix prefix, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
    unless skipLocalCache $ saveBinaryToLocalCache lCacheDir
                                                   (Zip.fromArchive frameworkArchive)
                                                   (prefix </> remoteFrameworkUploadPath)
                                                   frameworkNameWithFrameworkExtension
                                                   verbose
 where
  remoteFrameworkUploadPath           = remoteFrameworkPath useXcFrameworks platform reverseRomeMap f version
  frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f



-- | Saves a dSYM `Zip.Archive` to a local cache.
saveDsymToLocalCache
  :: FilePath -- ^ The cache definition.
  -> Zip.Archive -- ^ The zipped archive of the dSYM.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
  -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) IO ()
saveDsymToLocalCache lCacheDir dSYMArchive reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (CachePrefix prefix, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
    unless skipLocalCache $ saveBinaryToLocalCache lCacheDir
                                                   (Zip.fromArchive dSYMArchive)
                                                   (prefix </> remoteDsymUploadPath)
                                                   (fwn <> ".dSYM")
                                                   verbose
  where remoteDsymUploadPath = remoteDsymPath platform reverseRomeMap f version



-- | Saves a bcsymbolmap `Zip.Archive` to a local cache.
saveBcsymbolmapToLocalCache
  :: FilePath -- ^ The cache definition.
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> Zip.Archive -- ^ The zipped archive of the bcsymbolmap.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` to limit the operation to.
  -> ReaderT (CachePrefix, SkipLocalCacheFlag, Bool) IO ()
saveBcsymbolmapToLocalCache lCacheDir dwarfUUID dwarfArchive reverseRomeMap (FrameworkVersion f@(Framework _ _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (CachePrefix prefix, SkipLocalCacheFlag skipLocalCache, verbose) <- ask
    unless skipLocalCache $ saveBinaryToLocalCache lCacheDir
                                                   (Zip.fromArchive dwarfArchive)
                                                   (prefix </> remoteBcSymbolmapUploadPath)
                                                   (bcsymbolmapNameFrom dwarfUUID)
                                                   verbose
  where remoteBcSymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version



-- | Saves a ByteString to file in a given base directory.
saveBinaryToLocalCache
  :: MonadIO m
  => FilePath -- ^ The path of the base directory.
  -> LBS.ByteString -- ^ The `ByteString` to save.
  -> FilePath -- ^ The destination path inside the base directory.
  -> String -- ^ A colloquial name for the artifact printed when verbose is `True`.
  -> Bool -- ^ A verbosity flag.
  -> m ()
saveBinaryToLocalCache cachePath binaryZip destinationPath objectName verbose = do
  let sayFunc = if verbose then sayLnWithTime else sayLn
  when verbose $ sayLnWithTime $ "Copying " <> objectName <> " to: " <> finalPath
  liftIO $ saveBinaryToFile binaryZip finalPath
  sayFunc $ "Copied " <> objectName <> " to: " <> finalPath
  where finalPath = cachePath </> destinationPath



-- | Saves a list of .version files to a local cache
saveVersionFilesToLocalCache
  :: FilePath -- ^ The cache definition.
  -> [ProjectNameAndVersion] -- ^ The information used to derive the name and path for the .version file.
  -> ReaderT (CachePrefix, Bool) IO ()
saveVersionFilesToLocalCache lCacheDir = mapM_ (saveVersonFileToLocalCache lCacheDir)



-- | Saves a .version file to a local Cache
saveVersonFileToLocalCache
  :: FilePath -- ^ The cache definition.
  -> ProjectNameAndVersion -- ^ The information used to derive the name and path for the .version file.
  -> ReaderT (CachePrefix, Bool) IO ()
saveVersonFileToLocalCache lCacheDir projectNameAndVersion = do
  (cachePrefix, verbose) <- ask
  versionFileExists      <- liftIO $ doesFileExist versionFileLocalPath

  when versionFileExists $ do
    versionFileContent <- liftIO $ LBS.readFile versionFileLocalPath
    saveVersionFileBinaryToLocalCache lCacheDir cachePrefix versionFileContent projectNameAndVersion verbose
 where
  versionFileName      = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileLocalPath = carthageBuildDirectory </> versionFileName



-- | Saves a `LBS.ByteString` representing a .version file to a file.
saveVersionFileBinaryToLocalCache
  :: MonadIO m
  => FilePath -- ^ The destinationf file.
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> LBS.ByteString -- ^ The contents of the .version file
  -> ProjectNameAndVersion  -- ^ The information used to derive the name and path for the .version file.
  -> Bool -- ^ A flag controlling verbosity.
  -> m ()
saveVersionFileBinaryToLocalCache lCacheDir (CachePrefix prefix) versionFileContent projectNameAndVersion =
  saveBinaryToLocalCache lCacheDir versionFileContent (prefix </> versionFileRemotePath) versionFileName
 where
  versionFileName       = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileRemotePath = remoteVersionFilePath projectNameAndVersion

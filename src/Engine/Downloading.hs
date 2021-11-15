{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Downloading where

import           Caches.Common
import           Configuration                  ( carthageArtifactsBuildDirectoryForPlatform )
import           Control.Exception              ( try, catch, throw, displayException)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , runReaderT
                                                , withReaderT
                                                )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Carthage.TargetPlatform
import           Data.Either                    ( lefts )
import           Data.Monoid                    ( (<>) )
import           Data.Romefile                  ( Framework(..) )
import qualified Data.UUID                     as UUID
                                                ( UUID )
import           System.Directory
import           System.FilePath                ( (</>) )
import           System.IO.Error                ( isDoesNotExistError )
import           Types                   hiding ( version )
import           Utils
import           Xcode.DWARF
import qualified Turtle

-- | Retrieves a Framework using the engine and unzip the contents
getFrameworkFromEngine
  :: FilePath -- ^ The `FilePath` to the engine
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> FilePath -- ^ A temporary intermediate directory
  -> ExceptT String (ReaderT (CachePrefix, Bool, UUID.UUID) IO) LBS.ByteString
getFrameworkFromEngine enginePath useXcFrameworks reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform tmpDir = do
  (CachePrefix cachePrefix, verbose, uuid) <- ask
  let frameworkLocalPath = cachePrefix </> remoteFrameworkUploadPath
  mapExceptT (withReaderT (const (verbose, uuid))) (getArtifactFromEngine enginePath frameworkLocalPath fwn tmpDir)
  where remoteFrameworkUploadPath = remoteFrameworkPath useXcFrameworks platform reverseRomeMap f version


-- | Retrieves a .version file using the engine
getVersionFileFromEngine
  :: FilePath -- ^ The `FilePath` to the engine
  -> ProjectNameAndVersion
  -> FilePath -- ^ A temporary intermediate directory
  -> ExceptT String (ReaderT (CachePrefix, Bool, UUID.UUID) IO) LBS.ByteString
getVersionFileFromEngine enginePath projectNameAndVersion tmpDir = do
  (CachePrefix prefix, verbose, uuid) <- ask
  let finalVersionFileRemotePath = prefix </> versionFileRemotePath
  mapExceptT (withReaderT (const (verbose, uuid)))
    $ getArtifactFromEngine enginePath finalVersionFileRemotePath versionFileName tmpDir
 where
  versionFileName       = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileRemotePath = remoteVersionFilePath projectNameAndVersion


-- | Retrieves a bcsymbolmap using the engine
getBcsymbolmapWithEngine
  :: FilePath -- ^ The `FilePath` to the engine
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> FilePath -- ^ A temporary intermediate directory
  -> ExceptT String (ReaderT (CachePrefix, Bool, UUID.UUID) IO) LBS.ByteString
getBcsymbolmapWithEngine enginePath reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform dwarfUUID tmpDir
  = do
    (CachePrefix prefix, verbose, uuid) <- ask
    let finalRemoteBcsymbolmaploadPath = prefix </> remoteBcSymbolmapUploadPath
    mapExceptT (withReaderT (const (verbose, uuid)))
      $ getArtifactFromEngine enginePath finalRemoteBcsymbolmaploadPath symbolmapName tmpDir
 where
  remoteBcSymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version
  symbolmapName               = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID


-- | Retrieves a dSYM using the engine
getDSYMFromEngine
  :: FilePath -- ^ The `FilePath` to the engine
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> FilePath -- ^ A temporary intermediate directory
  -> ExceptT String (ReaderT (CachePrefix, Bool, UUID.UUID) IO) LBS.ByteString
getDSYMFromEngine enginePath reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform tmpDir = do
  (CachePrefix prefix, verbose, uuid) <- ask
  let finalRemoteDSYMUploadPath = prefix </> remoteDSYMUploadPath
  mapExceptT (withReaderT (const (verbose, uuid)))
    $ getArtifactFromEngine enginePath finalRemoteDSYMUploadPath dSYMName tmpDir
 where
  remoteDSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
  dSYMName             = fwn <> ".dSYM"


-- | Retrieves a bcsymbolmap using the engine and unzip the contents
getAndUnzipBcsymbolmapWithEngine
  :: FilePath -- ^ The `FilePath` to the engine
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> FilePath -- ^ A temporary intermediate directory
  -> ExceptT String (ReaderT (CachePrefix, Bool, UUID.UUID) IO) ()
getAndUnzipBcsymbolmapWithEngine enginePath reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) version) platform dwarfUUID tmpDir
  = when (platform `elem` fwps) $ do
    (_, verbose, _) <- ask
    let sayFunc       = if verbose then sayLnWithTime else sayLn
    let symbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID
    binary <- getBcsymbolmapWithEngine enginePath reverseRomeMap fVersion platform dwarfUUID tmpDir
    liftIO $ deleteFile (bcsymbolmapPath dwarfUUID) verbose
      `catch` (\e ->
        if isDoesNotExistError e then when verbose $ sayFunc ("Error :" <> displayException e) else throw e)
    unzipBinary binary symbolmapName (bcsymbolmapZipName dwarfUUID) verbose
 where
  platformBuildDirectory = carthageArtifactsBuildDirectoryForPlatform platform f
  bcsymbolmapZipName d = bcsymbolmapArchiveName d version
  bcsymbolmapPath d = platformBuildDirectory </> bcsymbolmapNameFrom d


-- | Retrieves all the bcsymbolmap files using the engine and unzip the contents
getAndUnzipBcsymbolmapsWithEngine'
  :: FilePath -- ^ The `FilePath` to the engine
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> FilePath -- ^ A temporary intermediate directory
  -> ExceptT DWARFOperationError (ReaderT (CachePrefix, Bool, UUID.UUID) IO) ()
getAndUnzipBcsymbolmapsWithEngine' enginePath reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) _) platform tmpDir
  = when (platform `elem` fwps) $ do

    dwarfUUIDs               <- withExceptT (const ErrorGettingDwarfUUIDs) $ dwarfUUIDsFrom (frameworkDirectory </> fwn)
    eitherDwarfUUIDsOrSucces <- forM
      dwarfUUIDs
      (\dwarfUUID -> lift $ runExceptT
        ( withExceptT (\e -> (dwarfUUID, e))
        $ getAndUnzipBcsymbolmapWithEngine enginePath reverseRomeMap fVersion platform dwarfUUID tmpDir
        )
      )

    let failedUUIDsAndErrors = lefts eitherDwarfUUIDsOrSucces
    unless (null failedUUIDsAndErrors) $ throwError $ FailedDwarfUUIDs failedUUIDsAndErrors
 where
  frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
  platformBuildDirectory              = carthageArtifactsBuildDirectoryForPlatform platform f
  frameworkDirectory                  = platformBuildDirectory </> frameworkNameWithFrameworkExtension


-- | Retrieves a Framework using the engine and unzip the contents
getAndUnzipFrameworkWithEngine
  :: FilePath -- ^ The `FilePath` to the engine
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> FilePath -- ^ A temporary intermediate directory
  -> ExceptT String (ReaderT (CachePrefix, Bool, UUID.UUID) IO) ()
getAndUnzipFrameworkWithEngine enginePath useXcFrameworks reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) version) platform tmpDir
  = when (platform `elem` fwps) $ do
    (_, verbose, _) <- ask
    frameworkBinary <- getFrameworkFromEngine enginePath useXcFrameworks reverseRomeMap fVersion platform tmpDir
    deleteFrameworkDirectory fVersion platform verbose
    unzipBinary frameworkBinary fwn frameworkZipName verbose
      <* ifExists frameworkExecutablePath (makeExecutable frameworkExecutablePath)
 where
  frameworkZipName        = frameworkArchiveName f version useXcFrameworks
  frameworkExecutablePath = frameworkBuildBundleForPlatform platform f </> fwn


-- | Retrieves a dSYM using the engine and unzip the contents
getAndUnzipDSYMWithEngine
  :: FilePath -- ^ The `FilePath` to the engine
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> FilePath -- ^ A temporary intermediate directory
  -> ExceptT String (ReaderT (CachePrefix, Bool, UUID.UUID) IO) ()
getAndUnzipDSYMWithEngine enginePath reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) version) platform tmpDir
  = when (platform `elem` fwps) $ do
    (_, verbose, _) <- ask
    dSYMBinary      <- getDSYMFromEngine enginePath reverseRomeMap fVersion platform tmpDir
    deleteDSYMDirectory fVersion platform verbose
    unzipBinary dSYMBinary fwn dSYMZipName verbose
  where dSYMZipName = dSYMArchiveName f version

-- | Retrieves an artifact using the engine
getArtifactFromEngine
  :: FilePath -- ^ The `FilePath` to the engine
  -> FilePath -- ^ The remote path 
  -> String -- ^ A colloquial name for the artifact
  -> FilePath -- ^ A temporary intermediate directory
  -> ExceptT String (ReaderT (Bool, UUID.UUID) IO) LBS.ByteString
getArtifactFromEngine enginePath remotePath artifactName tmpDir = do
  readerEnv <- ask
  eitherArtifact :: Either IOError LBS.ByteString <- liftIO $ Control.Exception.try $ runReaderT
    (downloadBinaryWithEngine enginePath remotePath artifactName tmpDir)
    readerEnv
  case eitherArtifact of
    Left  e              -> throwError $ "Error: could not download " <> artifactName <> " : " <> show e
    Right artifactBinary -> return artifactBinary

-- | Downloads an artifact stored at a given path using the engine
downloadBinaryWithEngine
  :: FilePath -- ^ The `FilePath` to the engine
  -> FilePath -- ^ The remote path
  -> String -- ^ A colloquial name for the artifact
  -> FilePath -- ^ A temporary intermediate directory
  -> ReaderT (Bool, UUID.UUID) IO LBS.ByteString
downloadBinaryWithEngine enginePath objectRemotePath objectName tmpDir = do
  (verbose, _) <- ask
  let cmd     = Turtle.fromString enginePath
  let sayFunc = if verbose then sayLnWithTime else sayLn
  when verbose
    $  sayLnWithTime
    $  "Invoking engine "
    <> show enginePath
    <> " to download "
    <> objectName
    <> " from: "
    <> objectRemotePath
  let outputPath = tmpDir </> objectRemotePath
  exitCode <- Turtle.proc cmd
                          ["download", Turtle.fromString objectRemotePath, Turtle.fromString outputPath]
                          (return $ Turtle.unsafeTextToLine "")
  case exitCode of
    Turtle.ExitSuccess   -> return ()
    Turtle.ExitFailure _ -> sayFunc $ "Error: could not download " <> outputPath
  binaryExists <- liftIO . doesFileExist $ outputPath
  if binaryExists
    then liftIO $ do
      binary <- LBS.readFile outputPath
      deleteFile outputPath verbose
        `catch` (\e -> let sayFuncIO = if verbose then sayLnWithTime else sayLn in
          if isDoesNotExistError e then when verbose $ sayFuncIO ("Error :" <> displayException e) else throw e)
      return binary
    else fail "Binary was not downloaded by engine"

{-# LANGUAGE OverloadedStrings #-}

module Engine.Downloading where

import           Caches.Common
import           Configuration                (carthageArtifactsBuildDirectoryForPlatform)
import           Control.Exception            (try)
import           Control.Lens                 (view)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader         (ReaderT, ask, runReaderT,
                                               withReaderT)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Carthage.TargetPlatform
import qualified Data.Conduit                 as C (ConduitT, await, yield,
                                                    (.|))
import qualified Data.Conduit.Binary          as C (sinkLbs)
import           Data.Either                  (lefts)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Romefile                (Framework (..))
import qualified Data.Text                    as T
import           System.FilePath              ((</>))
import           Types                        hiding (version)
import           Utils
import           Xcode.DWARF
import qualified Turtle

-- | Retrieves a Framework using the engine and unzip the contents
getFrameworkFromEngine
  :: FilePath -- ^ The engine file path
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT
       String
       (ReaderT (CachePrefix, Bool) IO)
       LBS.ByteString
getFrameworkFromEngine enginePath reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform
  = do
    (CachePrefix prefix, verbose) <- ask
    mapExceptT
      (withReaderT (const (verbose)))
      (getArtifactFromEngine enginePath (prefix </> remoteFrameworkUploadPath) fwn
      )
 where
  remoteFrameworkUploadPath =
    remoteFrameworkPath platform reverseRomeMap f version

-- | Retrieves a dSYM using the engine
getDSYMFromEngine
  :: FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT
       String
       (ReaderT (CachePrefix, Bool) IO)
       LBS.ByteString
getDSYMFromEngine enginePath reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform
  = do
    (CachePrefix prefix, verbose) <- ask
    let finalRemoteDSYMUploadPath = prefix </> remoteDSYMUploadPath
    mapExceptT (withReaderT (const (verbose)))
      $ getArtifactFromEngine enginePath finalRemoteDSYMUploadPath dSYMName
 where
  remoteDSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
  dSYMName             = fwn <> ".dSYM"

-- getVersionFileFromEngine
-- | Retrieves a .version file using the engine
getVersionFileFromEngine
  :: FilePath
  -> ProjectNameAndVersion
  -> ExceptT
       String
       (ReaderT (CachePrefix, Bool) IO)
       LBS.ByteString
getVersionFileFromEngine enginePath projectNameAndVersion = do
  (CachePrefix prefix, verbose) <- ask
  let finalVersionFileRemotePath = prefix </> versionFileRemotePath
  mapExceptT (withReaderT (const (verbose))) $ getArtifactFromEngine
    enginePath
    finalVersionFileRemotePath
    versionFileName
 where
  versionFileName = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileRemotePath = remoteVersionFilePath projectNameAndVersion

-- | Retrieves a bcsymbolmap using the engine
getBcsymbolmapFromEngine
  :: FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> ExceptT
       String
       (ReaderT (CachePrefix, Bool) IO)
       LBS.ByteString
getBcsymbolmapFromEngine enginePath reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform dwarfUUID
  = do
    (CachePrefix prefix, verbose) <- ask
    let finalRemoteBcsymbolmaploadPath = prefix </> remoteBcSymbolmapUploadPath
    mapExceptT (withReaderT (const (verbose))) $ getArtifactFromEngine
      enginePath
      finalRemoteBcsymbolmaploadPath
      symbolmapName
 where
  remoteBcSymbolmapUploadPath =
    remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version
  symbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID

-- | Retrieves a Framework using the engine and unzip the contents
getAndUnzipFrameworkWithEngine
  :: FilePath -- ^ The path to the engine
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (CachePrefix, Bool) IO) ()
getAndUnzipFrameworkWithEngine enginePath reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (_, verbose) <- ask
    frameworkBinary <- getFrameworkFromEngine enginePath
                                          reverseRomeMap
                                          fVersion
                                          platform
    deleteFrameworkDirectory fVersion platform verbose
    unzipBinary frameworkBinary fwn frameworkZipName verbose
      <* ifExists
           frameworkExecutablePath
           (makeExecutable frameworkExecutablePath)
 where
  frameworkZipName        = frameworkArchiveName f version
  frameworkExecutablePath = frameworkBuildBundleForPlatform platform f </> fwn

-- | Retrieves a dSYM using the engine and unzip the contents
getAndUnzipDSYMWithEngine
  :: FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (CachePrefix, Bool) IO) ()
getAndUnzipDSYMWithEngine enginePath reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = undefined
  -- = when (platform `elem` fwps) $ do
  --   (_, _, verbose) <- ask
  --   dSYMBinary <- getDSYMFromS3 s3BucketName reverseRomeMap fVersion platform
  --   deleteDSYMDirectory fVersion platform verbose
  --   unzipBinary dSYMBinary fwn dSYMZipName verbose
  -- where dSYMZipName = dSYMArchiveName f version

-- | Retrieves all the bcsymbolmap files using the engine and unzip the contents
getAndUnzipBcsymbolmapsWithEngine'
  :: FilePath -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT
       DWARFOperationError
       (ReaderT (CachePrefix, Bool) IO)
       ()
getAndUnzipBcsymbolmapsWithEngine' enginePath reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) _) platform
  = undefined
 --  = when (platform `elem` fwps) $ do

 --    dwarfUUIDs <- withExceptT (const ErrorGettingDwarfUUIDs)
 --      $ dwarfUUIDsFrom (frameworkDirectory </> fwn)
 --    eitherDwarfUUIDsOrSucces <- forM
 --      dwarfUUIDs
 --      (\dwarfUUID -> lift $ runExceptT
 --        (withExceptT (\e -> (dwarfUUID, e)) $ getAndUnzipBcsymbolmapFromS3
 --          lCacheDir
 --          reverseRomeMap
 --          fVersion
 --          platform
 --          dwarfUUID
 --        )
 --      )

 --    let failedUUIDsAndErrors = lefts eitherDwarfUUIDsOrSucces
 --    unless (null failedUUIDsAndErrors) $ throwError $ FailedDwarfUUIDs
 --      failedUUIDsAndErrors
 -- where
 --  frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
 --  platformBuildDirectory =
 --    carthageArtifactsBuildDirectoryForPlatform platform f
 --  frameworkDirectory =
 --    platformBuildDirectory </> frameworkNameWithFrameworkExtension

-- | Retrieves an artifact using the engine
getArtifactFromEngine
  :: FilePath -- ^ The engine file path
  -> FilePath -- ^ The path in the cache
  -> String -- ^ A colloquial name for the artifact
  -> ExceptT String (ReaderT (Bool) IO) LBS.ByteString
getArtifactFromEngine enginePath remotePath artifactName = do
  readerEnv@(verbose)            <- ask
  eitherArtifact <- liftIO $ try $ runReaderT
    (downloadBinaryWithEngine enginePath remotePath artifactName)
    readerEnv
  case eitherArtifact of
    Left e ->
      throwError
        $  "Error: could not download "
        <> artifactName
        <> " : "
        <> awsErrorToString e verbose
    Right artifactBinary -> return artifactBinary

-- | Downloads an artifact stored at a given path using the engine
downloadBinaryWithEngine
  :: FilePath
  -> FilePath
  -> FilePath
  -> ReaderT (Bool) IO LBS.ByteString
downloadBinaryWithEngine enginePath objectRemotePath objectName = do
    (verbose) <- ask
    let cmd = Turtle.fromString $ enginePath
    (exitCode) <- Turtle.proc
        cmd
        ["download", (Turtle.fromString objectRemotePath), (Turtle.fromString objectName)]
        (return $ Turtle.unsafeTextToLine "")
    case exitCode of
        Turtle.ExitSuccess   -> return ()
        Turtle.ExitFailure n -> Turtle.die (cmd <> " failed with exit code: " <> Turtle.repr n)
    undefined

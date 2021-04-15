{-# LANGUAGE OverloadedStrings #-}

module Engine.Uploading where

import qualified Codec.Archive.Zip             as Zip
import           Control.Monad                            ( when )
import           Control.Monad.Reader                     ( ReaderT
                                                          , ask
                                                          , withReaderT
                                                          )
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy          as LBS
import           Data.Carthage.TargetPlatform
import           Data.Monoid                              ( (<>) )
import           Data.Romefile                            ( Framework(..) )
import           System.FilePath                          ( (</>) )
import           Types                             hiding ( version )
import           Utils
import           Xcode.DWARF
import qualified Turtle


-- | Uploads a Framework `Zip.Archive` to an engine.
uploadFrameworkToEngine
  :: Zip.Archive -- ^ The `Zip.Archive` of the Framework.
  -> FilePath -- ^ The `FilePath` to the engine.
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework.
  -> TargetPlatform -- ^ A `TargetPlatform`s restricting the scope of this action.
  -> ReaderT (CachePrefix, Bool) IO ()
uploadFrameworkToEngine frameworkArchive enginePath useXcFrameworks reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (CachePrefix prefix, verbose) <- ask
    withReaderT (const verbose)
      $ uploadBinary enginePath (Zip.fromArchive frameworkArchive) (prefix </> remoteFrameworkUploadPath) fwn
  where remoteFrameworkUploadPath = remoteFrameworkPath useXcFrameworks platform reverseRomeMap f version



-- | Uploads a dSYM `Zip.Archive` to an engine.
uploadDsymToEngine
  :: Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
  -> FilePath -- ^ The `FilePath` to the engine.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT (CachePrefix, Bool) IO ()
uploadDsymToEngine dSYMArchive enginePath reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform =
  when (platform `elem` fwps) $ do
    (CachePrefix prefix, verbose) <- ask
    withReaderT (const verbose)
      $ uploadBinary enginePath (Zip.fromArchive dSYMArchive) (prefix </> remoteDsymUploadPath) (fwn <> ".dSYM")
  where remoteDsymUploadPath = remoteDsymPath platform reverseRomeMap f version



-- | Uploads a bcsymbolmap `Zip.Archive` to an engine.
uploadBcsymbolmapToEngine
  :: DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
  -> FilePath -- ^ The `FilePath` to the engine.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT (CachePrefix, Bool) IO ()
uploadBcsymbolmapToEngine dwarfUUID dwarfArchive enginePath reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (CachePrefix prefix, verbose) <- ask
    withReaderT (const verbose) $ uploadBinary enginePath
                                               (Zip.fromArchive dwarfArchive)
                                               (prefix </> remoteBcsymbolmapUploadPath)
                                               (fwn <> "." <> bcsymbolmapNameFrom dwarfUUID)
  where remoteBcsymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version



-- | Uploads a .version file using an engine
uploadVersionFileToEngine'
  :: FilePath -- ^ The `FilePath` to the engine.
  -> LBS.ByteString -- ^ The contents of the .version file.
  -> ProjectNameAndVersion -- ^ The information used to derive the name and path for the .version file.
  -> ReaderT (CachePrefix, Bool) IO ()
uploadVersionFileToEngine' enginePath versionFileContent projectNameAndVersion = do
  (CachePrefix prefix, verbose) <- ask
  withReaderT (const verbose)
    $ uploadBinary enginePath versionFileContent (prefix </> versionFileRemotePath) versionFileName
 where
  versionFileName       = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileRemotePath = remoteVersionFilePath projectNameAndVersion



-- | Uploads an artifact using an engine
uploadBinary
  :: MonadIO a
  => FilePath -- ^ The `FilePath` to the engine.
  -> LBS.ByteString
  -> FilePath
  -> FilePath
  -> ReaderT Bool a ()
uploadBinary enginePath binaryZip destinationPath _ = do
  verbose <- ask
  let cmd = Turtle.fromString enginePath
  liftIO $ saveBinaryToFile binaryZip destinationPath
  when verbose $ sayLnWithTime $ "Invoking engine " <> show enginePath <> " to upload " <> destinationPath
  exitCode <- Turtle.proc cmd
                          ["upload", Turtle.fromString destinationPath, Turtle.fromString destinationPath]
                          (return $ Turtle.unsafeTextToLine "")
  case exitCode of
    Turtle.ExitSuccess   -> return ()
    Turtle.ExitFailure n -> sayLn $ "Error " <> show n <> ": could not upload " <> destinationPath


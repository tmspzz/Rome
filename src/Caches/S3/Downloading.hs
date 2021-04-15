module Caches.S3.Downloading where

import           Caches.Common
import           Configuration                  ( carthageArtifactsBuildDirectoryForPlatform )
import           Control.Exception              ( try
                                                , catch
                                                , throw
                                                , displayException
                                                )
import           Control.Lens                   ( view )
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , runReaderT
                                                , withReaderT
                                                )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Carthage.TargetPlatform
import qualified Data.Conduit                  as C
                                                ( ConduitT
                                                , await
                                                , yield
                                                , (.|)
                                                )
import qualified Data.Conduit.Binary           as C
                                                ( sinkLbs )
import           Data.Either                    ( lefts )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>) )
import           Data.Romefile                  ( Framework(..) )
import qualified Data.Text                     as T
import qualified Network.AWS                   as AWS
import qualified Network.AWS.S3                as S3
import           System.FilePath                ( (</>) )
import           System.IO.Error                ( isDoesNotExistError )
import           Types                   hiding ( version )
import           Utils
import           Xcode.DWARF



-- | Retrieves a Framework from an S3 Cache and unzip the contents
getFrameworkFromS3
  :: S3.BucketName -- ^ The cache definition
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
getFrameworkFromS3 s3BucketName useXcFrameworks reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform = do
  (env, CachePrefix prefix, verbose) <- ask
  mapExceptT (withReaderT (const (env, verbose)))
             (getArtifactFromS3 s3BucketName (prefix </> remoteFrameworkUploadPath) fwn)
  where remoteFrameworkUploadPath = remoteFrameworkPath useXcFrameworks platform reverseRomeMap f version



-- | Retrieves a dSYM from an S3 Cache
getDSYMFromS3
  :: S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
getDSYMFromS3 s3BucketName reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform = do
  (env, CachePrefix prefix, verbose) <- ask
  let finalRemoteDSYMUploadPath = prefix </> remoteDSYMUploadPath
  mapExceptT (withReaderT (const (env, verbose))) $ getArtifactFromS3 s3BucketName finalRemoteDSYMUploadPath dSYMName
 where
  remoteDSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
  dSYMName             = fwn <> ".dSYM"



-- | Retrieves a .version file from S3
getVersionFileFromS3
  :: S3.BucketName -> ProjectNameAndVersion -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
getVersionFileFromS3 s3BucketName projectNameAndVersion = do
  (env, CachePrefix prefix, verbose) <- ask
  let finalVersionFileRemotePath = prefix </> versionFileRemotePath
  mapExceptT (withReaderT (const (env, verbose)))
    $ getArtifactFromS3 s3BucketName finalVersionFileRemotePath versionFileName
 where
  versionFileName       = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileRemotePath = remoteVersionFilePath projectNameAndVersion



-- | Retrieves a bcsymbolmap from an S3 Cache
getBcsymbolmapFromS3
  :: S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
getBcsymbolmapFromS3 s3BucketName reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform dwarfUUID =
  do
    (env, CachePrefix prefix, verbose) <- ask
    let finalRemoteBcsymbolmaploadPath = prefix </> remoteBcSymbolmapUploadPath
    mapExceptT (withReaderT (const (env, verbose)))
      $ getArtifactFromS3 s3BucketName finalRemoteBcsymbolmaploadPath symbolmapName
 where
  remoteBcSymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version
  symbolmapName               = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID



-- | Retrieves a Framework from an S3 Cache and unzip the contents
getAndUnzipFrameworkFromS3
  :: S3.BucketName -- ^ The cache definition
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipFrameworkFromS3 s3BucketName useXcFrameworks reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (_, _, verbose) <- ask
    frameworkBinary <- getFrameworkFromS3 s3BucketName useXcFrameworks reverseRomeMap fVersion platform
    deleteFrameworkDirectory fVersion platform verbose
    unzipBinary frameworkBinary fwn frameworkZipName verbose
      <* ifExists frameworkExecutablePath (makeExecutable frameworkExecutablePath)
 where
  frameworkZipName        = frameworkArchiveName f version useXcFrameworks
  frameworkExecutablePath = frameworkBuildBundleForPlatform platform f </> fwn



-- | Retrieves a dSYM from an S3 Cache and unzip the contents
getAndUnzipDSYMFromS3
  :: S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipDSYMFromS3 s3BucketName reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (_, _, verbose) <- ask
    dSYMBinary      <- getDSYMFromS3 s3BucketName reverseRomeMap fVersion platform
    deleteDSYMDirectory fVersion platform verbose
    unzipBinary dSYMBinary fwn dSYMZipName verbose
  where dSYMZipName = dSYMArchiveName f version



-- | Retrieves a bcsymbolmap from an S3 Cache and unzip the contents
getAndUnzipBcsymbolmapFromS3
  :: S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipBcsymbolmapFromS3 s3BucketName reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) version) platform dwarfUUID
  = when (platform `elem` fwps) $ do
    (_, _, verbose) <- ask
    let sayFunc       = if verbose then sayLnWithTime else sayLn
    let symbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID
    binary <- getBcsymbolmapFromS3 s3BucketName reverseRomeMap fVersion platform dwarfUUID
    liftIO
      $       deleteFile (bcsymbolmapPath dwarfUUID) verbose
      `catch` (\e -> if isDoesNotExistError e then when verbose $ sayFunc ("Error :" <> displayException e) else throw e
              )
    unzipBinary binary symbolmapName (bcsymbolmapZipName dwarfUUID) verbose
 where
  platformBuildDirectory = carthageArtifactsBuildDirectoryForPlatform platform f
  bcsymbolmapZipName d = bcsymbolmapArchiveName d version
  bcsymbolmapPath d = platformBuildDirectory </> bcsymbolmapNameFrom d



-- | Retrieves all the bcsymbolmap files from S3 and unzip the contents
getAndUnzipBcsymbolmapsFromS3'
  :: S3.BucketName -- ^ The cache definition
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT DWARFOperationError (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
getAndUnzipBcsymbolmapsFromS3' lCacheDir reverseRomeMap fVersion@(FrameworkVersion f@(Framework fwn _ fwps) _) platform
  = when (platform `elem` fwps) $ do

    dwarfUUIDs               <- withExceptT (const ErrorGettingDwarfUUIDs) $ dwarfUUIDsFrom (frameworkDirectory </> fwn)
    eitherDwarfUUIDsOrSucces <- forM
      dwarfUUIDs
      (\dwarfUUID -> lift $ runExceptT
        ( withExceptT (\e -> (dwarfUUID, e))
        $ getAndUnzipBcsymbolmapFromS3 lCacheDir reverseRomeMap fVersion platform dwarfUUID
        )
      )

    let failedUUIDsAndErrors = lefts eitherDwarfUUIDsOrSucces
    unless (null failedUUIDsAndErrors) $ throwError $ FailedDwarfUUIDs failedUUIDsAndErrors
 where
  frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
  platformBuildDirectory              = carthageArtifactsBuildDirectoryForPlatform platform f
  frameworkDirectory                  = platformBuildDirectory </> frameworkNameWithFrameworkExtension



-- | Retrieves an artifact from an S3 Cache
getArtifactFromS3
  :: S3.BucketName -- ^ The cache definition
  -> FilePath -- ^ The path in the cache
  -> String -- ^ A colloquial name for the artifact
  -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
getArtifactFromS3 s3BucketName remotePath artifactName = do
  readerEnv@(_, verbose) <- ask
  eitherArtifact         <- liftIO $ try $ runReaderT (downloadBinary s3BucketName remotePath artifactName) readerEnv
  case eitherArtifact of
    Left e -> throwError $ "Error: could not download " <> artifactName <> " : " <> awsErrorToString e verbose
    Right artifactBinary -> return artifactBinary



-- | Downloads an artifact stored at a given path from an `S3.BucketName`.
downloadBinary :: S3.BucketName -> FilePath -> FilePath -> ReaderT (AWS.Env, Bool) IO LBS.ByteString
downloadBinary s3BucketName objectRemotePath objectName = do
  (env, verbose) <- ask
  AWS.runResourceT . AWS.runAWS env $ do
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose $ sayFunc $ "Started downloading " <> objectName <> " from: " <> objectRemotePath
    rs <- AWS.send $ S3.getObject s3BucketName objectKey
    let contentLength = fromIntegral $ fromMaybe 0 $ view S3.gorsContentLength rs
    binary <- view S3.gorsBody rs `AWS.sinkBody` sink verbose contentLength
    sayFunc $ "Downloaded " <> objectName <> " from: " <> objectRemotePath
    return binary
 where
  objectKey = S3.ObjectKey . T.pack $ objectRemotePath
  sink verbose totalLength = if verbose then printProgress objectName totalLength C..| C.sinkLbs else C.sinkLbs

  printProgress :: MonadIO m => String -> Int -> C.ConduitT BS.ByteString BS.ByteString m ()
  printProgress objName totalLength = loop totalLength 0 0
   where
    loop t consumedLen lastLen = C.await >>= maybe
      (return ())
      (\bs -> do
        let len                = consumedLen + BS.length bs
        let diffGreaterThan1MB = len - lastLen >= 1024 * 1024
        when (diffGreaterThan1MB || len == t)
          $  sayLnWithTime
          $  "Downloaded "
          <> showInMegabytes len
          <> " of "
          <> showInMegabytes totalLength
          <> " for "
          <> objName
        C.yield bs
        let a = if diffGreaterThan1MB then len else lastLen
        loop t len a
      )


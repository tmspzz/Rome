module Caches.S3.Uploading where

import qualified Codec.Archive.Zip             as Zip
import           Control.Monad                            ( when )
import           Control.Monad.Reader                     ( ReaderT
                                                          , ask
                                                          , withReaderT
                                                          )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Carthage.TargetPlatform
import           Data.Monoid                              ( (<>) )
import           Data.Romefile                            ( Framework(..) )
import qualified Data.Text                     as T
import qualified Network.AWS                   as AWS
import qualified Network.AWS.S3                as S3
import           System.FilePath                          ( (</>) )
import           Types                             hiding ( version )
import           Utils
import           Xcode.DWARF



-- | Uploads a Framework `Zip.Archive` to an S3 Bucket.
uploadFrameworkToS3
  :: Zip.Archive -- ^ The `Zip.Archive` of the Framework.
  -> S3.BucketName -- ^ The cache definition.
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework.
  -> TargetPlatform -- ^ A `TargetPlatform`s restricting the scope of this action.
  -> ReaderT UploadDownloadEnv IO ()
uploadFrameworkToS3 frameworkArchive s3BucketName useXcFrameworks reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (env, CachePrefix prefix, verbose) <- ask
    withReaderT (const (env, verbose))
      $ uploadBinary s3BucketName (Zip.fromArchive frameworkArchive) (prefix </> remoteFrameworkUploadPath) fwn
  where remoteFrameworkUploadPath = remoteFrameworkPath useXcFrameworks platform reverseRomeMap f version



-- | Uploads a dSYM `Zip.Archive` to an S3 Bucket.
uploadDsymToS3
  :: Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
  -> S3.BucketName -- ^ The cache definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT UploadDownloadEnv IO ()
uploadDsymToS3 dSYMArchive s3BucketName reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform =
  when (platform `elem` fwps) $ do
    (env, CachePrefix prefix, verbose) <- ask
    withReaderT (const (env, verbose))
      $ uploadBinary s3BucketName (Zip.fromArchive dSYMArchive) (prefix </> remoteDsymUploadPath) (fwn <> ".dSYM")
  where remoteDsymUploadPath = remoteDsymPath platform reverseRomeMap f version



-- | Uploads a bcsymbolmap `Zip.Archive` to an S3 Bucket.
uploadBcsymbolmapToS3
  :: DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
  -> S3.BucketName -- ^ The cache definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT UploadDownloadEnv IO ()
uploadBcsymbolmapToS3 dwarfUUID dwarfArchive s3BucketName reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = when (platform `elem` fwps) $ do
    (env, CachePrefix prefix, verbose) <- ask
    withReaderT (const (env, verbose)) $ uploadBinary s3BucketName
                                                      (Zip.fromArchive dwarfArchive)
                                                      (prefix </> remoteBcsymbolmapUploadPath)
                                                      (fwn <> "." <> bcsymbolmapNameFrom dwarfUUID)
  where remoteBcsymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version



-- | Uploads a .version file to an S3 Bucket
uploadVersionFileToS3
  :: S3.BucketName -- ^ The cache definition.
  -> LBS.ByteString -- ^ The contents of the .version file.
  -> ProjectNameAndVersion -- ^ The information used to derive the name and path for the .version file.
  -> ReaderT (AWS.Env, CachePrefix, Bool) IO ()
uploadVersionFileToS3 s3BucketName versionFileContent projectNameAndVersion = do
  (env, CachePrefix prefix, verbose) <- ask
  withReaderT (const (env, verbose))
    $ uploadBinary s3BucketName versionFileContent (prefix </> versionFileRemotePath) versionFileName
 where

  versionFileName       = versionFileNameForProjectName $ fst projectNameAndVersion
  versionFileRemotePath = remoteVersionFilePath projectNameAndVersion



-- | Uploads an artifact to an `S3.BucketName` at a given path in the bucket.
uploadBinary :: AWS.ToBody a => S3.BucketName -> a -> FilePath -> FilePath -> ReaderT (AWS.Env, Bool) IO ()
uploadBinary s3BucketName binaryZip destinationPath objectName = do
  (env, verbose) <- ask
  let objectKey = S3.ObjectKey $ T.pack destinationPath
  AWS.runResourceT . AWS.runAWS env $ do
    let body    = AWS.toBody binaryZip
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose $ sayFunc $ "Started uploading " <> objectName <> " to: " <> destinationPath
    rs <- AWS.trying AWS._Error (AWS.send $ S3.putObject s3BucketName objectKey body)
    case rs of
      Left  e -> sayFunc $ "Error uploading " <> objectName <> ": " <> awsErrorToString e verbose
      Right _ -> sayFunc $ "Uploaded " <> objectName <> " to: " <> destinationPath


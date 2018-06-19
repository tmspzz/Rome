module Caches.Bintray.Downloading where

import           Control.Arrow          (left)
import           Control.Exception      as E (try)
import           Control.Monad.Except
import qualified Data.ByteString.Lazy   as LBS
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.HTTP.Conduit   as HTTP

-- | Get a binary from Bintray https://dl.bintray.com
downloadBinary :: MonadIO m
               => Text -- ^ path to the binary, including subject, repo and filepath
               -> ExceptT String m LBS.ByteString
downloadBinary artifactLocation = do
  req <- liftIO $ HTTP.parseRequest ("https://dl.bintray.com/" <> T.unpack artifactLocation)
  manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  eitherBody <- liftIO $ (left (show :: HTTP.HttpException -> String)) <$> E.try (HTTP.responseBody <$> HTTP.httpLbs req manager)
  either throwError return eitherBody



-- getFrameworkFromBintray :: MonadIO m
--                         => Bintray.Subject
--                         -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
--                         -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
--                         -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
--                         -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
-- -- | Retrieves a Framework from an S3 Cache and unzip the contents
-- getFrameworkFromS3 :: S3.BucketName -- ^ The cache definition
--                    -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
--                    -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
--                    -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
--                    -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
-- getFrameworkFromS3 s3BucketName
--                    reverseRomeMap
--                    (FrameworkVersion f@(FrameworkName fwn) version)
--                    platform = do
--   (env, CachePrefix prefix, verbose) <- ask
--   mapExceptT (withReaderT (const (env, verbose)))
--              (getArtifactFromS3 s3BucketName (prefix </> remoteFrameworkUploadPath) fwn)
--   where
--     remoteFrameworkUploadPath = remoteFrameworkPath platform reverseRomeMap f version



-- -- | Retrieves a dSYM from an S3 Cache
-- getDSYMFromS3 :: S3.BucketName -- ^ The cache definition
--               -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
--               -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
--               -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
--               -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
-- getDSYMFromS3 s3BucketName
--               reverseRomeMap
--               (FrameworkVersion f@(FrameworkName fwn) version)
--               platform = do
--   (env, CachePrefix prefix, verbose) <-  ask
--   let finalRemoteDSYMUploadPath = prefix </> remoteDSYMUploadPath
--   mapExceptT (withReaderT (const (env, verbose))) $
--               getArtifactFromS3 s3BucketName finalRemoteDSYMUploadPath dSYMName
--   where
--     remoteDSYMUploadPath = remoteDsymPath platform reverseRomeMap f version
--     dSYMName = fwn <> ".dSYM"



-- -- | Retrieves a .version file from S3
-- getVersionFileFromS3 :: S3.BucketName
--                      -> GitRepoNameAndVersion
--                      -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
-- getVersionFileFromS3 s3BucketName
--                      gitRepoNameAndVersion = do
--   (env, CachePrefix prefix, verbose) <- ask
--   let finalVersionFileRemotePath = prefix </> versionFileRemotePath
--   mapExceptT (withReaderT (const (env, verbose))) $
--     getArtifactFromS3 s3BucketName finalVersionFileRemotePath versionFileName
--   where
--     versionFileName = versionFileNameForGitRepoName $ fst gitRepoNameAndVersion
--     versionFileRemotePath = remoteVersionFilePath gitRepoNameAndVersion



-- -- | Retrieves a bcsymbolmap from an S3 Cache
-- getBcsymbolmapFromS3 :: S3.BucketName -- ^ The cache definition
--                      -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
--                      -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
--                      -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
--                      -> DwarfUUID -- ^ The UUID of the bcsymblmap
--                      -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) LBS.ByteString
-- getBcsymbolmapFromS3 s3BucketName
--                      reverseRomeMap
--                      (FrameworkVersion f@(FrameworkName fwn) version)
--                      platform
--                      dwarfUUID = do
--   (env, CachePrefix prefix, verbose) <-  ask
--   let finalRemoteBcsymbolmaploadPath = prefix </> remoteBcSymbolmapUploadPath
--   mapExceptT (withReaderT (const (env, verbose))) $
--               getArtifactFromS3 s3BucketName finalRemoteBcsymbolmaploadPath symbolmapName
--   where
--     remoteBcSymbolmapUploadPath = remoteBcsymbolmapPath dwarfUUID platform reverseRomeMap f version
--     symbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID



-- -- | Retrieves a Framework from an S3 Cache and unzip the contents
-- getAndUnzipFrameworkFromS3 :: S3.BucketName -- ^ The cache definition
--                            -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
--                            -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
--                            -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
--                            -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
-- getAndUnzipFrameworkFromS3 s3BucketName
--                            reverseRomeMap
--                            fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
--                            platform = do
--     (_, _, verbose) <- ask
--     frameworkBinary <- getFrameworkFromS3 s3BucketName reverseRomeMap fVersion platform
--     deleteFrameworkDirectory fVersion platform verbose
--     unzipBinary frameworkBinary fwn frameworkZipName verbose
--                              <* makeExecutable platform f
--   where
--     frameworkZipName = frameworkArchiveName f version



-- -- | Retrieves a dSYM from an S3 Cache and unzip the contents
-- getAndUnzipDSYMFromS3 :: S3.BucketName -- ^ The cache definition
--                       -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
--                       -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
--                       -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
--                       -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
-- getAndUnzipDSYMFromS3 s3BucketName
--                       reverseRomeMap
--                       fVersion@(FrameworkVersion f@(FrameworkName fwn) version)
--                       platform = do
--     (_, _, verbose) <- ask
--     dSYMBinary <- getDSYMFromS3 s3BucketName reverseRomeMap fVersion platform
--     deleteDSYMDirectory fVersion platform verbose
--     unzipBinary dSYMBinary fwn dSYMZipName verbose
--   where
--       dSYMZipName = dSYMArchiveName f version



-- -- | Retrieves a bcsymblmap from an S3 Cache and unzip the contents
-- getAndUnzipBcsymbolmapFromS3 :: S3.BucketName -- ^ The cache definition
--                              -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
--                              -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM
--                              -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
--                              -> DwarfUUID -- ^ The UUID of the bcsymblmap
--                              -> ExceptT String (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
-- getAndUnzipBcsymbolmapFromS3 s3BucketName
--                              reverseRomeMap
--                              fVersion@(FrameworkVersion (FrameworkName fwn) version)
--                              platform
--                              dwarfUUID = do
--     (_, _, verbose) <- ask
--     let symbolmapName = fwn <> "." <> bcsymbolmapNameFrom dwarfUUID
--     binary <- getBcsymbolmapFromS3 s3BucketName reverseRomeMap fVersion platform dwarfUUID
--     deleteFile (bcsybolmapPath dwarfUUID) verbose
--     unzipBinary binary symbolmapName (bcsymbolmapZipName dwarfUUID) verbose
--   where
--       platformBuildDirectory = carthageBuildDirectoryForPlatform platform
--       bcsymbolmapZipName d = bcsymbolmapArchiveName d version
--       bcsybolmapPath d = platformBuildDirectory </> bcsymbolmapNameFrom d



-- -- | Retrieves all the bcsymbolmap files from S3 and unzip the contents
-- getAndUnzipBcsymbolmapsFromS3' :: S3.BucketName -- ^ The cache definition
--                                -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the dSYM in the cache
--                                -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
--                                -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
--                                -> ExceptT DWARFOperationError (ReaderT (AWS.Env, CachePrefix, Bool) IO) ()
-- getAndUnzipBcsymbolmapsFromS3' lCacheDir
--                                reverseRomeMap
--                                fVersion@(FrameworkVersion f@(FrameworkName fwn) _)
--                                platform = do

--   dwarfUUIDs <- withExceptT (const ErrorGettingDwarfUUIDs) $ dwarfUUIDsFrom (frameworkDirectory </> fwn)
--   eitherDwarfUUIDsOrSucces <- forM dwarfUUIDs
--     (\dwarfUUID ->
--       lift $ runExceptT (withExceptT
--         (\e -> (dwarfUUID, e)) $
--           getBcsymbolmapFromS3 lCacheDir reverseRomeMap fVersion platform dwarfUUID))

--   let failedUUIDsAndErrors = lefts eitherDwarfUUIDsOrSucces
--   unless (null failedUUIDsAndErrors) $
--       throwError $ FailedDwarfUUIDs failedUUIDsAndErrors

--   where
--     frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
--     platformBuildDirectory = carthageBuildDirectoryForPlatform platform
--     frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension



-- -- | Retrieves an artifact from an S3 Cache
-- getArtifactFromS3 :: S3.BucketName -- ^ The cache definition
--                   -> FilePath -- ^ The path in the cache
--                   -> String -- ^ A colloquial name for the artifact
--                   -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
-- getArtifactFromS3 s3BucketName
--                   remotePath
--                   name = do
--   eitherArtifact <- AWS.trying AWS._Error $ downloadBinary s3BucketName remotePath name
--   case eitherArtifact of
--     Left e -> throwError $ "Error: could not download " <> name <> " : " <> awsErrorToString e
--     Right artifactBinary -> return artifactBinary

module Engine.Downloading where

import qualified Turtle
import           Control.Monad.Except
import           Caches.Common
import           Data.Carthage.TargetPlatform
import           System.FilePath
import           Utils
import           Types                   hiding ( version )
import           Control.Monad.Reader         (ReaderT, ask, runReaderT,
                                               withReaderT)
import qualified Data.Text                    as T
import qualified Network.AWS                  as AWS
import qualified Network.AWS.S3               as S3
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Romefile                (Framework (..))
import           Control.Exception            (try)
import           Control.Lens                 (view)
import           Data.Maybe                   (fromMaybe)


-- | Retrieves a Framework from an S3 Cache and unzip the contents
getFrameworkFromEngine
  :: FilePath -- ^ The engine file path
  -> InvertedRepositoryMap -- ^ The map used to resolve from a `FrameworkVersion` to the path of the Framework in the cache
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework
  -> TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
  -> ExceptT
       String
       (ReaderT (AWS.Env, CachePrefix, Bool) IO)
       LBS.ByteString
getFrameworkFromEngine enginePath reverseRomeMap (FrameworkVersion f@(Framework fwn _ _) version) platform
  = do
    (env, CachePrefix prefix, verbose) <- ask
    mapExceptT
      (withReaderT (const (env, verbose)))
      (getArtifactFromEngine enginePath (prefix </> remoteFrameworkUploadPath) fwn
      )
 where
  remoteFrameworkUploadPath =
    remoteFrameworkPath platform reverseRomeMap f version


-- | Retrieves an artifact from an S3 Cache
getArtifactFromEngine
  :: FilePath -- ^ The engine file path
  -> FilePath -- ^ The path in the cache
  -> String -- ^ A colloquial name for the artifact
  -> ExceptT String (ReaderT (AWS.Env, Bool) IO) LBS.ByteString
getArtifactFromEngine enginePath remotePath artifactName = undefined --do
  -- readerEnv@(_, verbose)            <- ask
  -- eitherArtifact <- liftIO $ try $ runReaderT
  --   (downloadBinaryWithEngine enginePath remotePath artifactName)
  --   readerEnv
  -- case eitherArtifact of
  --   Left e ->
  --     throwError
  --       $  "Error: could not download "
  --       <> artifactName
  --   Right artifactBinary -> return artifactBinary


-- | Downloads an artificat stored at a given path from an `S3.BucketName`.
downloadBinaryWithEngine
  :: FilePath
  -> FilePath
  -> FilePath
  -> ReaderT (AWS.Env, Bool) IO LBS.ByteString
downloadBinaryWithEngine enginePath objectRemotePath objectName = do
  (env, verbose) <- ask
  do
    let sayFunc = if verbose then sayLnWithTime else sayLn
    when verbose
      $  sayFunc
      $  "Started downloading "
      <> objectName
      <> " from: "
      <> objectRemotePath
    -- rs <- AWS.send $ S3.getObject enginePath objectKey
    -- let contentLength =
          -- fromIntegral $ fromMaybe 0 $ view S3.gorsContentLength rs
    -- binary <- view S3.gorsBody rs `AWS.sinkBody` sink verbose contentLength
    sayFunc $ "Downloaded " <> objectName <> " from: " <> objectRemotePath
    return undefined -- return binary
 where
  objectKey = S3.ObjectKey . T.pack $ objectRemotePath

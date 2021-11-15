module Caches.S3.Probing where

import           Control.Concurrent.Async.Lifted.Safe     ( mapConcurrently )
import           Control.Monad.Reader                     ( ReaderT
                                                          , ask
                                                          )
import           Data.Carthage.TargetPlatform
import           Data.List                                ( intersect )
import           Data.Romefile                            ( _frameworkPlatforms )
import qualified Data.Text                     as T
import qualified Network.AWS                   as AWS
import qualified Network.AWS.S3                as S3
import           System.FilePath                          ( (</>) )
import           Types
import           Utils



-- | Probes the caches described by `RomeCacheInfo` to check whether a list of `FrameworkVersion` is present or not
-- | in the caches for each `TargetPlatform`
probeS3ForFrameworks
  :: S3.BucketName -- ^ The cache definition.
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
  -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
  -> ReaderT (AWS.Env, CachePrefix, Bool) IO [FrameworkAvailability]
probeS3ForFrameworks s3BucketName useXcFrameworks reverseRomeMap frameworkVersions platforms = mapConcurrently probe frameworkVersions
  where probe fVersions = probeS3ForFramework s3BucketName useXcFrameworks reverseRomeMap fVersions platforms



-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not in each `TargetPlatform`
probeS3ForFramework
  :: S3.BucketName -- ^ The cache definition.
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
  -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
  -> ReaderT (AWS.Env, CachePrefix, Bool) IO FrameworkAvailability
probeS3ForFramework s3BucketName useXcFrameworks reverseRomeMap frameworkVersion platforms = fmap
  (FrameworkAvailability frameworkVersion)
  probeForEachPlatform
 where
  probeForEachPlatform = mapConcurrently
    (probeS3ForFrameworkOnPlatform s3BucketName useXcFrameworks reverseRomeMap frameworkVersion)
    (platforms `intersect` (_frameworkPlatforms . _framework $ frameworkVersion))



-- | Probes the caches described by `RomeCacheInfo` to check whether a `FrameworkVersion` is present or not for a `TargetPlatform`.
probeS3ForFrameworkOnPlatform
  :: S3.BucketName -- ^ The cache definition.
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
  -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
  -> ReaderT (AWS.Env, CachePrefix, Bool) IO PlatformAvailability
probeS3ForFrameworkOnPlatform s3BucketName useXcFrameworks reverseRomeMap (FrameworkVersion fwn v) platform = do
  (env, CachePrefix prefixStr, _) <- ask
  let isAvailable = AWS.runResourceT . AWS.runAWS env $ checkIfFrameworkExistsInBucket
        s3BucketName
        (frameworkObjectKeyWithPrefix prefixStr)
  PlatformAvailability platform <$> isAvailable
 where
  frameworkObjectKeyWithPrefix cPrefix =
    S3.ObjectKey . T.pack $ cPrefix </> remoteFrameworkPath useXcFrameworks platform reverseRomeMap fwn v



-- | Probes a `S3.BucketName` to check whether an `S3.ObjectKey` is present or not.
checkIfFrameworkExistsInBucket
  :: AWS.MonadAWS m
  => S3.BucketName -- ^ The name of the bucket.
  -> S3.ObjectKey -- ^ The `S3.ObjectKey` to look for.
  -> m Bool
checkIfFrameworkExistsInBucket s3BucketName frameworkObjectKey = do
  rs <- AWS.trying AWS._Error (AWS.send $ S3.headObject s3BucketName frameworkObjectKey)
  case rs of
    Left  _ -> return False
    Right _ -> return True

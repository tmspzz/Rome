module Caches.Local.Probing where



import           Control.Monad.IO.Class
import           Data.Carthage.TargetPlatform
import           Data.List                                ( intersect )
import           Data.Romefile                            ( _frameworkPlatforms )
import           System.Directory
import           System.FilePath                          ( (</>) )
import           Types                             hiding ( version )
import           Utils

-- | Probes a `FilePath` to check if each `FrameworkVersion` exists for each `TargetPlatform`
probeLocalCacheForFrameworks
  :: MonadIO m
  => FilePath -- ^ The cache definition.
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
  -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
  -> m [FrameworkAvailability]
probeLocalCacheForFrameworks lCacheDir cachePrefix useXcFrameworks reverseRomeMap frameworkVersions = sequence . probeForEachFramework
 where
  probeForEachFramework = mapM (probeLocalCacheForFramework lCacheDir cachePrefix useXcFrameworks reverseRomeMap) frameworkVersions



-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for each `TargetPlatform`
probeLocalCacheForFramework
  :: MonadIO m
  => FilePath -- ^ The cache definition.
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
  -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
  -> m FrameworkAvailability
probeLocalCacheForFramework lCacheDir cachePrefix useXcFrameworks reverseRomeMap frameworkVersion platforms = fmap
  (FrameworkAvailability frameworkVersion)
  probeForEachPlatform
 where
  probeForEachPlatform = mapM
    (probeLocalCacheForFrameworkOnPlatform lCacheDir cachePrefix useXcFrameworks reverseRomeMap frameworkVersion)
    (platforms `intersect` (_frameworkPlatforms . _framework $ frameworkVersion))



-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for a given `TargetPlatform`
probeLocalCacheForFrameworkOnPlatform
  :: MonadIO m
  => FilePath -- ^ The cache definition.
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> Bool -- ^ useXcFrameworks
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
  -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
  -> m PlatformAvailability
probeLocalCacheForFrameworkOnPlatform lCacheDir (CachePrefix prefix) useXcFrameworks reverseRomeMap (FrameworkVersion fwn version) platform
  = do
    frameworkExistsInLocalCache <- liftIO . doesFileExist $ frameworkLocalCachePath
    return (PlatformAvailability platform frameworkExistsInLocalCache)
 where
  frameworkLocalCachePath   = lCacheDir </> prefix </> remoteFrameworkUploadPath
  remoteFrameworkUploadPath = remoteFrameworkPath useXcFrameworks platform reverseRomeMap fwn version

module Caches.Local.Probing where



import           Control.Monad.IO.Class
import           Data.Carthage.TargetPlatform
import           System.Directory
import           System.FilePath              ((</>))
import           Types                        hiding (version)
import           Utils

-- | Probes a `FilePath` to check if each `FrameworkVersion` exists for each `TargetPlatform`
probeLocalCacheForFrameworks
  :: MonadIO m
  => FilePath -- ^ The chache definition.
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
  -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
  -> m [FrameworkAvailability]
probeLocalCacheForFrameworks lCacheDir cachePrefix reverseRomeMap frameworkVersions
  = sequence . probeForEachFramework
 where
  probeForEachFramework = mapM
    (probeLocalCacheForFramework lCacheDir cachePrefix reverseRomeMap)
    frameworkVersions



-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for each `TargetPlatform`
probeLocalCacheForFramework
  :: MonadIO m
  => FilePath -- ^ The chache definition.
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
  -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
  -> m FrameworkAvailability
probeLocalCacheForFramework lCacheDir cachePrefix reverseRomeMap frameworkVersion platforms
  = fmap (FrameworkAvailability frameworkVersion) probeForEachPlatform
 where
  probeForEachPlatform = mapM
    (probeLocalCacheForFrameworkOnPlatform lCacheDir
                                           cachePrefix
                                           reverseRomeMap
                                           frameworkVersion
    )
    platforms



-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for a given `TargetPlatform`
probeLocalCacheForFrameworkOnPlatform
  :: MonadIO m
  => FilePath -- ^ The chache definition.
  -> CachePrefix -- ^ A prefix for folders at top level in the cache.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
  -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
  -> m PlatformAvailability
probeLocalCacheForFrameworkOnPlatform lCacheDir (CachePrefix prefix) reverseRomeMap (FrameworkVersion fwn version) platform
  = do
    frameworkExistsInLocalCache <-
      liftIO . doesFileExist $ frameworkLocalCachePath
    return (PlatformAvailability platform frameworkExistsInLocalCache)
 where
  frameworkLocalCachePath = lCacheDir </> prefix </> remoteFrameworkUploadPath
  remoteFrameworkUploadPath =
    remoteFrameworkPath platform reverseRomeMap fwn version




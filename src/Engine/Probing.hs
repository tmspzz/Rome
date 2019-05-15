{-# LANGUAGE OverloadedStrings #-}

module Engine.Probing where

import           Control.Monad.IO.Class
import           Data.Carthage.TargetPlatform
import           Data.List                    (intersect)
import           Data.Romefile                (_frameworkPlatforms)
import           Types                        hiding (version)
import           Utils
import qualified Turtle


-- | Probes a `FilePath` to check if each `FrameworkVersion` exists for each `TargetPlatform`
probeEngineForFrameworks
  :: MonadIO m
  => FilePath -- ^ The engine path.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> [FrameworkVersion] -- ^ A list of `FrameworkVersion` to probe for.
  -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
  -> m [FrameworkAvailability]
probeEngineForFrameworks lCacheDir reverseRomeMap frameworkVersions
  = sequence . probeForEachFramework
 where
  probeForEachFramework = mapM
    (probeEngineForFramework lCacheDir reverseRomeMap)
    frameworkVersions


-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for each `TargetPlatform`
probeEngineForFramework
  :: MonadIO m
  => FilePath -- ^ The engine path.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
  -> [TargetPlatform] -- ^ A list target platforms restricting the scope of this action.
  -> m FrameworkAvailability
probeEngineForFramework lCacheDir reverseRomeMap frameworkVersion platforms
  = fmap (FrameworkAvailability frameworkVersion) probeForEachPlatform
 where
  probeForEachPlatform = mapM
    (probeEngineForFrameworkOnPlatform lCacheDir
                                       reverseRomeMap
                                       frameworkVersion
   )
    (platforms `intersect` (_frameworkPlatforms . _framework $ frameworkVersion))


-- | Probes a `FilePath` to check if a `FrameworkVersion` exists for a given `TargetPlatform`
probeEngineForFrameworkOnPlatform
  :: MonadIO m
  => FilePath -- ^ The engine path.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` to probe for.
  -> TargetPlatform -- ^ A target platforms restricting the scope of this action.
  -> m PlatformAvailability
probeEngineForFrameworkOnPlatform enginePath reverseRomeMap (FrameworkVersion fwn version) platform
  = do
    let cmd = Turtle.fromString $ enginePath
    (exitCode) <- Turtle.proc
        cmd
        ["list", (Turtle.fromString remoteFrameworkUploadPath)]
        (return $ Turtle.unsafeTextToLine "")
    case exitCode of
        -- If script exits with success, we assume the framework exists.
        Turtle.ExitSuccess   -> return (PlatformAvailability platform True)
        Turtle.ExitFailure n -> return (PlatformAvailability platform False)
 where
  remoteFrameworkUploadPath =
    remoteFrameworkPath platform reverseRomeMap fwn version


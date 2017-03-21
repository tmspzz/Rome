module Types where

import           Control.Monad.Except         (ExceptT)
import           Data.Carthage.Cartfile       (Version)
import           Data.Carthage.TargetPlatform
import qualified Data.Map.Strict              as M
import           Data.Romefile                (FrameworkName, GitRepoName)
import qualified Network.AWS.Env              as AWS (Env)
import           Types.Commands


type UDCEnv                = (AWS.Env{-, VerifyFlag-}, SkipLocalCacheFlag, Bool)
type RomeMonad             = ExceptT String IO
type RepositoryMap         = M.Map GitRepoName [FrameworkName]
type InvertedRepositoryMap = M.Map FrameworkName GitRepoName

type GitRepoNameAndVersion = (GitRepoName, Version)


-- | Represents the name of a framework together with its version
data FrameworkVersion = FrameworkVersion { _frameworkName    :: FrameworkName
                                         , _frameworkVersion :: Version
                                         }
                                         deriving (Show, Eq)

-- | Represents the availability of a framework for a given `TargetPlatform`
data PlatformAvailability = PlatformAvailability { _availabilityPlatform :: TargetPlatform
                                                , _isAvailable           :: Bool
                                                }
                                                deriving (Show, Eq)

-- | Represents the availablity of a given `GitRepoName` at a given `Version`
-- | as a list of `PlatformAvailability`s
data GitRepoAvailability = GitRepoAvailability { _availabilityRepo         :: GitRepoName
                                             , _availabilityVersion        :: Version
                                             , _repoPlatformAvailabilities :: [PlatformAvailability]
                                             }
                                             deriving (Show, Eq)

-- | Represents the availability of a `FrameworkVersion` as a list of
-- | `PlatformAvailability`s
data FrameworkAvailability = FrameworkAvailability { _availabilityFramework         :: FrameworkVersion
                                                 , _frameworkPlatformAvailabilities :: [PlatformAvailability]
                                                 }
                                                 deriving (Show, Eq)

{-# LANGUAGE DeriveGeneric #-}
module Types where

import           Control.Monad.Except         (ExceptT)
import           Data.Aeson
import           Data.Carthage.Cartfile       (Version)
import           Data.Carthage.TargetPlatform
import qualified Data.Map.Strict              as M
import           Data.Romefile                (Framework, ProjectName)
import           GHC.Generics
import qualified Network.AWS.Env              as AWS (Env)
import           Types.Commands




type UploadDownloadCmdEnv  = (AWS.Env, CachePrefix, SkipLocalCacheFlag, ConcurrentlyFlag, Bool)
type UploadDownloadEnv     = (AWS.Env, CachePrefix, Bool)
type RomeMonad             = ExceptT String IO
type RepositoryMap         = M.Map ProjectName [Framework]
type InvertedRepositoryMap = M.Map Framework ProjectName

type RomeVersion           = (Int, Int, Int, Int)

type ProjectNameAndVersion = (ProjectName, Version)

-- | A wrapper around `String` used to specify what prefix to user
-- | when determining remote paths of artifacts

newtype CachePrefix = CachePrefix { _unCachePrefix :: String }
                                  deriving (Show, Eq)

-- | Represents the name of a framework together with its version
data FrameworkVersion = FrameworkVersion { _framework        :: Framework
                                         , _frameworkVersion :: Version
                                         }
                                         deriving (Show, Eq)

-- | Represents the availability of a framework for a given `TargetPlatform`
data PlatformAvailability = PlatformAvailability { _availabilityPlatform :: TargetPlatform
                                                 , _isAvailable          :: Bool
                                                 }
                                                 deriving (Show, Eq)

-- | Represents the availablity of a given `GitRepoName` at a given `Version`
-- | as a list of `PlatformAvailability`s
data ProjectAvailability = ProjectAvailability { _availabilityProject        :: ProjectName
                                               , _availabilityVersion        :: Version
                                               , _repoPlatformAvailabilities :: [PlatformAvailability]
                                               }
                                               deriving (Show, Eq)

-- | Represents the availability of a `FrameworkVersion` as a list of
-- | `PlatformAvailability`s
data FrameworkAvailability = FrameworkAvailability { _availabilityFramework           :: FrameworkVersion
                                                   , _frameworkPlatformAvailabilities :: [PlatformAvailability]
                                                   }
                                                   deriving (Show, Eq)

data RepoJSON = RepoJSON { name    :: String
                         , version :: String
                         , present :: [String]
                         , missing :: [String]
                         }
                         deriving (Show, Eq, Generic)

instance ToJSON RepoJSON where

newtype ReposJSON = ReposJSON [RepoJSON] deriving (Show, Eq, Generic)

instance ToJSON ReposJSON where

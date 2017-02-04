{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Data.GitRepoAvailability
    ( GitRepoAvailability (..)
    , FrameworkAvailability (..)
    , FrameworkVersion (..)
    , PlatformAvailability (..)
    ) where

import Data.Cartfile
import Data.Romefile
import Data.TargetPlatform

data GitRepoAvailability = GitRepoAvailability { _availabilityRepo           :: GitRepoName
                                               , _availabilityVersion        :: Version
                                               , _repoPlatformAvailabilities :: [PlatformAvailability]
                                               }
                                               deriving (Show, Eq)

data FrameworkAvailability = FrameworkAvailability { _availabilityFramework           :: FrameworkVersion
                                                   , _frameworkPlatformAvailabilities :: [PlatformAvailability]
                                                   }
                                                   deriving (Show, Eq)

data FrameworkVersion = FrameworkVersion { _frameworkName    :: FrameworkName
                                         , _frameworkVersion :: Version
                                         }
                                         deriving (Show, Eq)

data PlatformAvailability = PlatformAvailability { _availabilityPlatform :: TargetPlatform
                                                 , _isAvailable          :: Bool
                                                 }
                                                 deriving (Show, Eq)

{-# LANGUAGE OverloadedStrings #-}

module Data.Carthage.VersionFile where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.Carthage.Common
import           Data.Carthage.TargetPlatform
import qualified Data.Map.Strict              as M


type FrameworkPlatformInfoMap = M.Map TargetPlatform [FrameworkPlatformInfo]


data FrameworkInfo = FrameworkInfo { _hash          :: String
                                   , _frameworkName :: String
                                   }
                                  deriving (Show, Eq)

instance FromJSON FrameworkInfo where
  parseJSON (Object v) = FrameworkInfo <$>
                       v .: "hash" <*>
                       v .: "name"
  parseJSON invalid    = typeMismatch "FrameworkInfo" invalid

data FrameworkPlatformInfo = FrameworkPlatformInfo { targetPlatform :: TargetPlatform
                                                   , hash           :: String
                                                   , frameworkName  :: String
                                                   }
                                                   deriving (Show, Eq)

data VersionFileEntry = VersionFileEntry { commitish    :: Version
                                         , xcodeVersion :: String
                                         , iOSFramewoksInfo :: Maybe [FrameworkInfo]
                                         , tvOSFrameworksInfo :: Maybe [FrameworkInfo]
                                         , watchOSFrameworksInfo :: Maybe [FrameworkInfo]
                                         , macOSFrameworksInfo :: Maybe [FrameworkInfo]
                                         }
                                         deriving (Show, Eq)

instance FromJSON VersionFileEntry where
  parseJSON (Object v) = VersionFileEntry
                      <$> (Version <$> v .: "commitish")
                      <*> v .: "xcodeVersion"
                      <*> v .:? "iOS"
                      <*> v .:? "tvOS"
                      <*> v .:? "watchOS"
                      <*> v .:? "Mac"

  parseJSON invalid    = typeMismatch "VersionFileEntry" invalid

{-# LANGUAGE OverloadedStrings #-}

module Data.Carthage.VersionFile where


import           Data.Carthage.Common
import           Data.Aeson
import           Data.Aeson.Types



data FrameworkInfo = FrameworkInfo { _hash                  :: String
                                   , _frameworkName         :: String
                                   , _swiftToolChainVersion :: Maybe String
                                   }
                                  deriving (Show, Eq)

instance FromJSON FrameworkInfo where
  parseJSON (Object v) = FrameworkInfo <$>
                       v .: "hash" <*>
                       v .: "name" <*>
                       v .:? "swiftToolchainVersion"
  parseJSON invalid    = typeMismatch "FrameworkInfo" invalid

data VersionFileEntry = VersionFileEntry { commitish    :: Version
                                         , iOSFramewoksInfo :: Maybe [FrameworkInfo]
                                         , tvOSFrameworksInfo :: Maybe [FrameworkInfo]
                                         , watchOSFrameworksInfo :: Maybe [FrameworkInfo]
                                         , macOSFrameworksInfo :: Maybe [FrameworkInfo]
                                         }
                                         deriving (Show, Eq)

instance FromJSON VersionFileEntry where
  parseJSON (Object v) = VersionFileEntry
                      <$> (Version <$> v .: "commitish")
                      <*> v .:? "iOS"
                      <*> v .:? "tvOS"
                      <*> v .:? "watchOS"
                      <*> v .:? "Mac"
  parseJSON invalid    = typeMismatch "VersionFileEntry" invalid

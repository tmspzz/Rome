{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Data.TargetPlatform
    ( allTargetPlatforms
    , readTargetPlatform
    , targetPlatformName
    , TargetPlatform (..)
    ) where


import           Control.Monad
import           Data.Char
import           Data.Maybe

data TargetPlatform = IOS | MacOS | TVOS | WatchOS
              deriving (Ord, Eq, Show)

targetPlatformName :: TargetPlatform -> String
targetPlatformName IOS = "iOS"
targetPlatformName MacOS = "MacOS"
targetPlatformName TVOS = "tvOS"
targetPlatformName WatchOS = "watchOS"

allTargetPlatforms = [IOS, MacOS, TVOS, WatchOS]

readTargetPlatform :: String -> Maybe TargetPlatform
readTargetPlatform str = listToMaybe matchingPlatforms
  where
    lowercaseStr = toLower <$> str
    matchingPlatforms = filter ((==lowercaseStr) . (liftM toLower) . targetPlatformName) allTargetPlatforms

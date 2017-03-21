module Data.Carthage.Common where

newtype Version  = Version { unVersion :: String }
                   deriving (Eq, Show, Ord)

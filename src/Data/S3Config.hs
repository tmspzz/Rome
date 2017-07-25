{-# LANGUAGE OverloadedStrings #-}
module Data.S3Config
  ( S3Config
  , parse
  , regionOf
  ) where

-- For now, only very little information needs to be extracted from the S3
-- config file, but extracting it into a separate module is consistent with
-- `Data.Romefile` and `Data.Carthage` and avoids dealing with the raw INI
-- file representation (String-keyed hashmaps) in the main logic.

import           Control.Monad       ((<=<))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Ini            (Ini, lookupValue, parseIni)
import qualified Network.AWS         as AWS
import qualified Network.AWS.Data    as AWS


newtype S3Config = S3Config { _ini :: Ini }

regionOf :: Text -> S3Config -> Either String AWS.Region
regionOf profile = parseRegion <=< lookupValue profile "region" . _ini
  where
    parseRegion s = if T.null s
      -- better error message
      then Left "Failed reading: Failure parsing Region from empty string"
      else AWS.fromText s

parse :: Text -> Either String S3Config
parse = fmap S3Config . parseIni

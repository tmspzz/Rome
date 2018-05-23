{-# LANGUAGE OverloadedStrings #-}

module Data.S3Config
  ( S3Config
  , parseS3Config
  , regionOf
  , endPointOf
  ) where

-- For now, only very little information needs to be extracted from the S3
-- config file, but extracting it into a separate module is consistent with
-- `Data.Romefile` and `Data.Carthage` and avoids dealing with the raw INI
-- file representation (String-keyed hashmaps) in the main logic.

import           Control.Monad     ((<=<))
import           Data.Either.Utils (maybeToEither)
import           Data.Ini          (Ini, lookupValue, parseIni)
import qualified Data.Text         as T (Text, null, unpack)
import qualified Network.AWS       as AWS
import qualified Network.AWS.Data  as AWS
import           Network.URL

newtype S3Config = S3Config { _ini :: Ini }

regionOf :: T.Text -> S3Config -> Either String AWS.Region
regionOf profile = parseRegion <=< lookupValue profile "region" . _ini
  where
    parseRegion s = if T.null s
      -- better error message
      then Left "Failed reading: Failure parsing Region from empty string"
      else AWS.fromText s

endPointOf :: T.Text -> S3Config -> Either String URL
endPointOf profile = parseURL <=< lookupValue profile "endpoint" . _ini
  where
    parseURL s = if T.null s
      then Left "Failed reading: Failure parsing Endpoint from empty string"
      else maybeToEither "Failed reading: Endpoint is not a valid URL" $ importURL . T.unpack $ s

parseS3Config :: T.Text -> Either String S3Config
parseS3Config = fmap S3Config . parseIni

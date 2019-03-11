{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Utils
  ( ConfigFile
  , CredentialsFile
  , parseConfigFile
  , parseCredentialsFile
  , regionOf
  , endPointOf
  , sourceProfileOf
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

newtype ConfigFile = ConfigFile { _awsConfigIni :: Ini }
newtype CredentialsFile = CredentialsFile { _awsCredentialsIni :: Ini }

class FromIni a where
  asIni :: a -> Ini

instance FromIni ConfigFile where
  asIni = _awsConfigIni

instance FromIni CredentialsFile where
  asIni = _awsCredentialsIni

regionOf :: T.Text -> ConfigFile -> Either String AWS.Region
regionOf profile = parseRegion <=< lookupValue profile "region" . asIni
 where
  parseRegion s = if T.null s
-- better error message
    then Left "Failed reading: Failure parsing Region from empty string"
    else AWS.fromText s

endPointOf :: T.Text -> ConfigFile -> Either String URL
endPointOf profile = parseURL <=< lookupValue profile "endpoint" . asIni
 where
  parseURL s = if T.null s
    then Left "Failed reading: Failure parsing Endpoint from empty string"
    else
      maybeToEither "Failed reading: Endpoint is not a valid URL"
      $ importURL
      . T.unpack
      $ s

sourceProfileOf :: T.Text ->  CredentialsFile -> Either String CredentialsFile
sourceProfileOf p credentialsFile = undefined

parseConfigFile :: T.Text -> Either String ConfigFile
parseConfigFile = fmap ConfigFile . parseIni

parseCredentialsFile :: T.Text -> Either String CredentialsFile
parseCredentialsFile = fmap CredentialsFile . parseIni

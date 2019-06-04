{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Utils
  ( ConfigFile
  , credentialsFromFile
  , configFromFile
  , authFromCredentilas
  , parseConfigFile
  , regionOf
  , endPointOf
  , sourceProfileOf
  , accessKeyIdOf
  , secretAccessKeyOf
  , roleARNOf
  ) where

-- For now, only very little information needs to be extracted from the S3
-- config file, but extracting it into a separate module is consistent with
-- `Data.Romefile` and `Data.Carthage` and avoids dealing with the raw INI
-- file representation (String-keyed hashmaps) in the main logic.

import           Control.Monad     ((<=<))
import           Data.Either.Utils (maybeToEither)
import           Data.Either.Extra (mapLeft)
import           Data.Ini          (Ini, lookupValue, parseIni)
import qualified Data.Text         as T (Text, null, unpack)
import qualified Data.Text.Encoding           as T (encodeUtf8)
import qualified Data.Text.IO      as T (readFile)
import qualified Network.AWS       as AWS
import qualified Network.AWS.Data  as AWS
import qualified Network.AWS.Data.Sensitive   as AWS (Sensitive (..))
import           Network.URL
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Except (ExceptT (..), withExceptT)

newtype ConfigFile = ConfigFile { _awsConfigIni :: Ini }
newtype CredentialsFile = CredentialsFile { _awsCredentialsIni :: Ini }

class FromIni a where
  asIni :: a -> Ini

instance FromIni ConfigFile where
  asIni = _awsConfigIni

instance FromIni CredentialsFile where
  asIni = _awsCredentialsIni

-- | Reads `CredentialsFile` from a file at a given path
credentialsFromFile
  :: MonadIO m
  => FilePath -- ^ The path to the file containing the credentials. Usually `~/.aws/credentials`
  -> ExceptT String m CredentialsFile
credentialsFromFile filePath = do
  file <- liftIO (T.readFile filePath)
  withExceptT (("Could not parse " <> filePath <> ": ") <>) (action file)
  where action a = ExceptT . return $ parseCredentialsFile a

-- | Reads `ConfigFile` from a file at a given path
configFromFile
  :: MonadIO m
  => FilePath -- ^ The path to the file containing the credentials. Usually `~/.aws/config`
  -> ExceptT String m ConfigFile
configFromFile filePath = do
  file <- liftIO (T.readFile filePath)
  withExceptT (("Could not parse " <> filePath <> ": ") <>) (action file)
  where action a = ExceptT . return $ parseConfigFile a

authFromCredentilas :: T.Text -> CredentialsFile -> Either String AWS.Auth
authFromCredentilas profile credentials = AWS.Auth <$> authEnv
 where
  accessKeyId     = T.encodeUtf8 <$> accessKeyIdOf profile credentials
  secretAccessKey = T.encodeUtf8 <$> secretAccessKeyOf profile credentials
  authEnv =
    AWS.AuthEnv
      <$> (AWS.AccessKey <$> accessKeyId)
      <*> (AWS.Sensitive . AWS.SecretKey <$> secretAccessKey)
      <*> pure Nothing
      <*> pure Nothing

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

getPropertyFromCredentials
  :: T.Text -> T.Text -> CredentialsFile -> Either String T.Text
getPropertyFromCredentials profile property =
  lookupValue profile property . asIni

getPropertyFromConfig
  :: T.Text -> T.Text -> ConfigFile -> Either String T.Text
getPropertyFromConfig profile property =
  lookupValue profile property . asIni

sourceProfileOf :: T.Text -> ConfigFile -> Either String T.Text
sourceProfileOf profile credFile =
  getPropertyFromConfig profile "source_profile" credFile
    `withError` const (missingKeyError key profile)
  where key = "source_profile"

roleARNOf :: T.Text -> ConfigFile -> Either String T.Text
roleARNOf profile credFile = getPropertyFromConfig profile key credFile
  `withError` const (missingKeyError key profile)
  where key = "role_arn"

accessKeyIdOf :: T.Text -> CredentialsFile -> Either String T.Text
accessKeyIdOf profile credFile =
  getPropertyFromCredentials profile key credFile
    `withError` const (missingKeyError key profile)
  where key = "aws_access_key_id"

missingKeyError :: T.Text -> T.Text -> String
missingKeyError key profile =
  "Could not find key `"
    ++ T.unpack key
    ++ "` for profile `"
    ++ T.unpack profile
    ++ "`"

withError :: Either a b -> (a -> c) -> Either c b
withError = flip mapLeft

secretAccessKeyOf :: T.Text -> CredentialsFile -> Either String T.Text
secretAccessKeyOf profile credFile =
  getPropertyFromCredentials profile key credFile
    `withError` const (missingKeyError key profile)
  where key = "aws_secret_access_key"

parseConfigFile :: T.Text -> Either String ConfigFile
parseConfigFile = fmap ConfigFile . parseIni

parseCredentialsFile :: T.Text -> Either String CredentialsFile
parseCredentialsFile = fmap CredentialsFile . parseIni


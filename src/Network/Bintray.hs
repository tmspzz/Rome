{-# LANGUAGE OverloadedStrings #-}

module Network.Bintray where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Monad.Except
import System.FilePath
import System.Directory
import Data.Monoid ((<>))

data Credentials = Credentials { _bintrayUser :: Text
                               , _bintrayAPIKey :: Text
                               }

discoverCredentials :: MonadIO m => ExceptT Text m Credentials
discoverCredentials = do
  liftIO $ getBintrayCredentialsFile >>=


getBintrayCredentialsFile :: MonadIO m => m FilePath
getBintrayCredentialsFile = liftIO $ (</> bintrayCredentialsFilePath) <$> getHomeDirectory
  where
      bintrayCredentialsFilePath = ".bintray/credentials"


-- | Read a file as `Text` and pefrom an action
fromFile :: MonadIO m
         => FilePath -- ^ The `FilePath` to the file to read
         -> (Text -> ExceptT Text m a) -- ^ The action
         -> ExceptT Text m a
fromFile f action = do
  file <- liftIO (T.readFile f)
  withExceptT (("Could not parse " <> T.pack f <> ": ") <>) (action file)

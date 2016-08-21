{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Ini.Utils
  (optionalKey
  , requireKey
  , inRequiredSection
  , inOptionalSection
  , fromIni'
  , fromIni''
  )
where

import           Data.Char
import           Data.Text              as T
import qualified Data.HashMap.Strict    as M
import           Data.Monoid
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Ini



requireKey :: (MonadReader (Ini, Text) m, MonadError Text m) => Text -> m Text
requireKey key = do
  (ini, section) <- ask
  case lookupValue section key ini of
    Left e -> invalidError Nothing (pack e)
    Right value
      | blank value -> invalidError (Just key) "cannot be a blank string."
      | otherwise -> return value
  where
    blank x = T.null x || T.all isSpace x

optionalKey :: Monad m => Text -> ReaderT (Ini, Text) m (Maybe Text)
optionalKey key = do
  (ini, section) <- ask
  case lookupValue section key ini of
    Left _ -> return Nothing
    Right value -> return $ Just value

requireSection :: (MonadReader Ini m, MonadError Text m) => Text -> m Text
requireSection section = do
  (Ini ini) <- ask
  case M.lookup section ini of
    Nothing -> invalidError Nothing ("Could not find section " <> section)
    Just _ -> return section

inRequiredSection ::  (MonadReader Ini m, MonadError Text m) => ReaderT (Ini, Text) m b -> Text -> m b
inRequiredSection reader section = do
  ini <- ask
  s <- requireSection section
  runReaderT reader (ini, s)

inOptionalSection :: (MonadReader Ini m) => Text -> a -> ReaderT (Ini, Text) (ExceptT Text m) a -> m a
inOptionalSection section defaultValue exceptingReader = do
  t <- runExceptT $ exceptingReader `inRequiredSection` section
  case t of
    Left _ -> return defaultValue
    Right a -> return a

invalidError Nothing  e = throwError e
invalidError (Just k) e = throwError $ "Key " <> k <> " " <> e

fromIni' :: ReaderT Ini (ExceptT Text m) a -> Ini -> m (Either Text a)
fromIni' r ini = runExceptT $ runReaderT r ini

fromIni'' :: ReaderT Ini m a -> Ini -> m a
fromIni'' = runReaderT


example :: Ini -> IO ()
example ini = do
  r <- requireKey "k" `inRequiredSection` "text" `fromIni'` ini
  t <- inOptionalSection "section" "default" (requireKey "k") `fromIni'` ini
  s <- inOptionalSection "section" (Just "default") (optionalKey "k") `fromIni''` ini
  case r of
    Right _ -> print r
    Left _  -> undefined

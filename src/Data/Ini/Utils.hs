{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Ini.Utils
  (optionalKey
  , requireKey
  , inRequiredSection
  , inOptionalSection
  , fromIni
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



{-|
  Require the presence of a `key` in a `section` and return the `key`'s `value`.
  The value of the `key` must not be an empty string.

  To signal an error and potentially stop the computation in which
  this is embedded, `requireKey` is inside `MonadError`.
  The error is represented as `Text`.

  `requireKey` works inside a `MonadReader` containing in the enviroment the
  `Ini` and the `secion` in which the `key` should be contained.

  In case of success it returns to the upper `m` the `value` as `Text`

  Also valid signature:
  `requireKey :: (MonadReader (Ini, Text) m, MonadError Text m) => Text -> m Text`
-}
-- requireKey :: Monad m => Text ->ReaderT (Ini, Text) (ExceptT Text m) Text
requireKey :: (MonadReader (Ini, Text) m, MonadError Text m) => Text -> m Text
requireKey key = do
  (ini, section) <- ask
  case lookupValue section key ini of
    Left e -> invalidError (Just key) (pack e)
    Right value
      | blank value -> invalidError (Just key) "cannot be a blank string."
      | otherwise -> return value
  where
    blank x = T.null x || T.all isSpace x


{-|
  Optionally check for the presence of `key` in a `section` and return `Just` the
  `key`'s `value` or `Nothing` .

  `optionalKey` works inside a `MonadReader` containing in the enviroment the
  `Ini` and the `secion` in which the `key` should be contained.

  In case of success it returns to the upper `m` the value as `Maybe Text`

  Also valid signature: `MonadReader (Ini, Text) m => Text -> m (Maybe Text)`
-}
optionalKey :: Monad m => Text -> ReaderT (Ini, Text) m (Maybe Text)
optionalKey key = do
  (ini, section) <- ask
  case lookupValue section key ini of
    Left _ -> return Nothing
    Right value -> return $ Just value

{-|
  Require the presence of `section` and return that `section`'s name if present.

  To signal an error and potentially stop the computation in which
  this is embedded, `requireSection` works inside `MonadError`.
  The error is represented as `Text`.

  `requireSection` works inside `MonadReader` containing in the enviroment the
  `Ini` in which the `secion` should be contained.

  In case of success it returns to the upper `m` the `section` as `Text`

  Also valid signature:
  `MonadReader Ini m, MonadError Text m) => Text -> m Text`
-}
requireSection :: (MonadReader Ini m, MonadError Text m) => Text -> m Text
requireSection section = do
  (Ini ini) <- ask
  case M.lookup section ini of
    Nothing -> invalidError Nothing ("Could not find section " <> section)
    Just _ -> return section

{-|
  Execture `f` if and only if `section` is present.

  To signal an error and potentially stop the computation in which
  this is embedded, `withSection` runs in `MonadError`.
  The error is represented as `Text`.

  `withSection` runs inside `MonadReader` containing in the enviroment the
  `Ini` in which the `secion` should be contained.
-}
inRequiredSection ::  (MonadReader Ini m, MonadError Text m) => ReaderT (Ini, Text) m b -> Text -> m b
inRequiredSection reader section = do
  ini <- ask
  s <- requireSection section
  runReaderT reader (ini, s)

{-|
  Execture `f` if and only if `section` is present. Otherwise return a
  `default` value.

  To signal an error and potentially stop the computation in which
  this is embedded, `m` is required to be the `MonadError`.
  The error is represented as `Text`.

  `m` is aslo required to be `MonadReader` containing in the enviroment the
  `Ini` in which the `secion` should be contained.
-}
inOptionalSection :: (MonadReader Ini m) => Text -> a -> ReaderT (Ini, Text) (ExceptT Text m) a -> m a
inOptionalSection section defaultValue exceptingReader = do
  t <- runExceptT $ exceptingReader `inRequiredSection` section
  case t of
    Left _ -> return defaultValue
    Right a -> return a

invalidError Nothing  e = throwError e
invalidError (Just k) e = throwError $ "key " <> k <> " " <> e

fromIni :: MonadIO m => ReaderT Ini (ExceptT Text m) b -> Ini -> m b
fromIni exceptingReader ini = do
  r <- exceptingReader `fromIni'` ini
  case r of
    Right t -> return t
    Left e -> throw $ userError (unpack e)

fromIni' :: ReaderT Ini (ExceptT Text m) a -> Ini -> m (Either Text a)
fromIni' r ini = runExceptT $ runReaderT r ini

fromIni'' :: ReaderT Ini m a -> Ini -> m a
fromIni'' = runReaderT


example :: Ini -> IO ()
example ini = do
  r <- requireKey "k" `inRequiredSection` "text" `fromIni'` ini
  t <- inOptionalSection "section" "default" (requireKey "k") `fromIni'` ini
  s <- inOptionalSection "section" (Just "default") (optionalKey "k") `fromIni''` ini
  k <- (optionalKey "k" `inRequiredSection` "section") `fromIni` ini
  q <- requireKey "k" `inRequiredSection` "text" `fromIni` ini

  case r of
    Right _ -> print r
    Left _  -> undefined

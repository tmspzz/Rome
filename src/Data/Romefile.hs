{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.Romefile
    ( parseRomefile
    , romefile
    , RomefileEntry (..)
    , FrameworkName
    , GitRepoName
    )
where

import           Data.Ini             as INI
import           Data.Ini.Utils       as INI
import           Data.HashMap.Strict  as M
import           Data.Monoid
import           Data.Text
import           Control.Monad.Except
import           Control.Monad.Trans



type FrameworkName = String
type GitRepoName   = String
data RomefileEntry = RomefileEntry { gitRepositoryName   :: GitRepoName
                                   , frameworkCommonName :: FrameworkName
                                   }
                                   deriving (Show, Eq)



-- |The name of the Romefile
romefile :: String
romefile = "Romefile"

-- |The delimiter of the CACHE section a Romefile
cacheSectionDelimiter :: Text
cacheSectionDelimiter = "CACHE"

-- |The S3-Bucket Key
s3BucketKey :: Text
s3BucketKey = "S3-Bucket"

-- |The delimier of the REPOSITORYMAP section
repositoryMapSectionDelimiter :: Text
repositoryMapSectionDelimiter = "REPOSITORYMAP"

parseRomefile ::  (MonadIO m, MonadError String m) => FilePath -> m (Text, [RomefileEntry])
parseRomefile f = do
  eitherIni <- liftIO $ INI.readIniFile f
  case eitherIni of
    Left iniError -> throwError iniError
    Right ini -> do
      eitherBucker <- getBucket ini
      case eitherBucker of
        Left e -> throwError $ "Error while parsing " <> f <> ": "  <> unpack e
        Right bucket -> do
          romeFileEntries <- getRomefileEntries ini
          return (bucket, romeFileEntries)

getBucket ini = requireKey s3BucketKey `inRequiredSection` cacheSectionDelimiter `fromIni'` ini

getRomefileEntries ini = do
  m <- inOptionalSection repositoryMapSectionDelimiter M.empty keysAndValues `fromIni''` ini
  return $ Prelude.map (\(repoName, frameworkCommonName) -> RomefileEntry (unpack repoName) (unpack frameworkCommonName)) (M.toList m)

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.Romefile
    ( parseRomefile
    , romefile
    , RomefileEntry (..)
    , FrameworkName (..)
    , GitRepoName (..)
    , RomeFileParseResult (..)
    )
where

import           Data.Ini             as INI
import           Data.Ini.Utils       as INI
import           Data.HashMap.Strict  as M
import           Data.Monoid
import           Data.Text
import           Control.Monad.Except
import           Control.Monad.Trans




newtype FrameworkName = FrameworkName { unFrameworkName :: String }
                        deriving (Eq, Show, Ord)

newtype GitRepoName   = GitRepoName { unGitRepoName :: String }
                        deriving (Eq, Show, Ord)

data RomefileEntry    = RomefileEntry { gitRepositoryName   :: GitRepoName
                                      , frameworkCommonNames :: [FrameworkName]
                                      }
                                      deriving (Show, Eq)

data RomeFileParseResult = RomeFileParseResult { bucket :: Text
                                               , repositoryMapEntries :: [RomefileEntry]
                                               , ignoreMapEntries :: [RomefileEntry]
                                               }



-- |The name of the Romefile
romefile :: String
romefile = "Romefile"

-- |The delimiter of the CACHE section a Romefile
cacheSectionDelimiter :: Text
cacheSectionDelimiter = "Cache"

-- |The S3-Bucket Key
s3BucketKey :: Text
s3BucketKey = "S3-Bucket"

-- |The delimier of the REPOSITORYMAP section
repositoryMapSectionDelimiter :: Text
repositoryMapSectionDelimiter = "RepositoryMap"

-- |The delimier of the IGNOREMAP section
ignoreMapSectionDelimiter :: Text
ignoreMapSectionDelimiter = "IgnoreMap"


parseRomefile ::  (MonadIO m, MonadError String m) => FilePath -> m RomeFileParseResult
parseRomefile f = do
  eitherIni <- liftIO $ INI.readIniFile f
  case eitherIni of
    Left iniError -> throwError iniError
    Right ini -> do
      eitherBucker <- getBucket ini
      case eitherBucker of
        Left e -> throwError $ "Error while parsing " <> f <> ": "  <> unpack e
        Right bucket -> do
          repositoryMapEntries <- getRepostiryMapEntries ini
          ignoreMapEntries <- getIgnoreMapEntries ini
          return RomeFileParseResult {..}

getBucket ini = requireKey s3BucketKey `inRequiredSection` cacheSectionDelimiter `fromIni'` ini

getRepostiryMapEntries :: MonadIO m => Ini -> m [RomefileEntry]
getRepostiryMapEntries = getRomefileEntries repositoryMapSectionDelimiter

getIgnoreMapEntries :: MonadIO m => Ini -> m [RomefileEntry]
getIgnoreMapEntries = getRomefileEntries ignoreMapSectionDelimiter

getRomefileEntries :: (MonadIO m) => Text -> Ini -> m [RomefileEntry]
getRomefileEntries sectionDelimiter ini = do
  m <- inOptionalSection sectionDelimiter M.empty keysAndValues `fromIni''` ini
  return $
    Prelude.map
    (\(repoName, frameworkCommonNames)
     -> RomefileEntry
        (GitRepoName (unpack repoName))
        (Prelude.map
         (FrameworkName . unpack . strip)
         (splitOn "," frameworkCommonNames)))
    (M.toList m)

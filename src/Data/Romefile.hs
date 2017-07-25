{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Data.Romefile
    ( parseRomefile
    , romefile
    , RomefileEntry (..)
    , FrameworkName (..)
    , GitRepoName (..)
    , RomeFileParseResult (..)
    , RomeCacheInfo (..)
    , cacheInfo
    , repositoryMapEntries
    , ignoreMapEntries
    , bucket
    , localCacheDir
    )
where

import           Control.Lens
import           Control.Monad.Except
import           Data.HashMap.Strict   as M
import           Data.Ini              as INI
import           Data.Maybe
import           Data.Monoid
import           Data.Text


newtype FrameworkName = FrameworkName { unFrameworkName :: String }
                                      deriving (Eq, Show, Ord)

newtype GitRepoName   = GitRepoName { unGitRepoName :: String }
                                    deriving (Eq, Show, Ord)

data RomefileEntry    = RomefileEntry { gitRepositoryName    :: GitRepoName
                                      , frameworkCommonNames :: [FrameworkName]
                                      }
                                      deriving (Eq, Show)

data RomeFileParseResult = RomeFileParseResult { _cacheInfo            :: RomeCacheInfo
                                               , _repositoryMapEntries :: [RomefileEntry]
                                               , _ignoreMapEntries     :: [RomefileEntry]
                                               }
                                               deriving (Eq, Show)

cacheInfo :: Lens' RomeFileParseResult RomeCacheInfo
cacheInfo = lens _cacheInfo (\parseResult n -> parseResult { _cacheInfo = n })

repositoryMapEntries :: Lens' RomeFileParseResult [RomefileEntry]
repositoryMapEntries = lens _repositoryMapEntries (\parseResult n -> parseResult { _repositoryMapEntries = n })

ignoreMapEntries :: Lens' RomeFileParseResult [RomefileEntry]
ignoreMapEntries = lens _ignoreMapEntries (\parseResult n -> parseResult { _ignoreMapEntries = n })



data RomeCacheInfo = RomeCacheInfo { _bucket        :: Maybe Text
                                   , _localCacheDir :: Maybe FilePath -- relative path
                                   }
                                   deriving (Eq, Show)

bucket :: Lens' RomeCacheInfo (Maybe Text)
bucket = lens _bucket (\cInfo n -> cInfo { _bucket = n })

localCacheDir :: Lens' RomeCacheInfo (Maybe FilePath)
localCacheDir = lens _localCacheDir (\cInfo n -> cInfo { _localCacheDir = n })



-- |The name of the Romefile
romefile :: String
romefile = "Romefile"

-- |The delimiter of the CACHE section a Romefile
cacheSectionDelimiter :: Text
cacheSectionDelimiter = "Cache"

-- |The S3-Bucket Key
s3BucketKey :: Text
s3BucketKey = "S3-Bucket"

-- |The local cache dir Key
localCacheDirKey :: Text
localCacheDirKey = "local"

-- |The delimier of the REPOSITORYMAP section
repositoryMapSectionDelimiter :: Text
repositoryMapSectionDelimiter = "RepositoryMap"

-- |The delimier of the IGNOREMAP section
ignoreMapSectionDelimiter :: Text
ignoreMapSectionDelimiter = "IgnoreMap"


parseRomefile :: Text -> Either String RomeFileParseResult
parseRomefile = toRomefile <=< INI.parseIni

toRomefile :: INI.Ini -> Either String RomeFileParseResult
toRomefile ini = do
  _bucket <- getBucket ini
  _localCacheDir <- getLocalCacheDir ini
  let _repositoryMapEntries = getRepositoryMapEntries ini
      _ignoreMapEntries = getIgnoreMapEntries ini
      _cacheInfo = RomeCacheInfo {..}
  return RomeFileParseResult {..}

getSection :: Text -> M.HashMap Text b -> Either String b
getSection key = maybe (Left err) Right . M.lookup key
  where
    err = "Could not find section: " <> show key

getBucket :: Ini -> Either String (Maybe Text)
getBucket (Ini ini) = M.lookup s3BucketKey <$> getSection cacheSectionDelimiter ini

getLocalCacheDir :: Ini -> Either String (Maybe FilePath)
getLocalCacheDir (Ini ini) =
  fmap unpack . M.lookup localCacheDirKey <$> getSection cacheSectionDelimiter ini

getRepositoryMapEntries :: Ini -> [RomefileEntry]
getRepositoryMapEntries = getRomefileEntries repositoryMapSectionDelimiter

getIgnoreMapEntries :: Ini -> [RomefileEntry]
getIgnoreMapEntries = getRomefileEntries ignoreMapSectionDelimiter

getRomefileEntries :: Text -> Ini -> [RomefileEntry]
getRomefileEntries sectionDelimiter (Ini ini) =
  fmap toEntry . M.toList . fromMaybe M.empty . M.lookup sectionDelimiter $ ini
  where
    toEntry :: (Text, Text) -> RomefileEntry
    toEntry (repoName, frameworkCommonNames) =
      RomefileEntry
        (GitRepoName (unpack repoName))
        (fmap (FrameworkName . unpack . strip) (splitOn "," frameworkCommonNames))


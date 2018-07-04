{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Data.Romefile
    ( parseRomefile
    , romefile
    , RomefileEntry (..)
    , Framework (..)
    , GitRepoName (..)
    , RomeFileParseResult (..)
    , RomeCacheInfo (..)
    , cacheInfo
    , repositoryMapEntries
    , ignoreMapEntries
    , bucket
    , localCacheDir
    , frameworkName
    , frameworkType
    )
where

import           Control.Lens
import           Control.Monad.Except
import qualified Data.HashMap.Strict  as M
import           Data.Ini             as INI
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as T
import           Text.Read
import qualified Text.Read.Lex        as L
import Data.Char


data FrameworkType = Dynamic
                   | Static deriving (Eq, Show, Ord)

instance Read FrameworkType where
  readPrec = parens $ do
    L.Ident s <- lexP
    case map toLower s of
      "dynamic" -> return Dynamic
      "static" -> return Static
      o -> fail $ "Could not parse \"" ++ o ++ "\" into a FrameworkType"

data Framework = Framework { _frameworkName :: String
                           , _frameworkType :: FrameworkType
                           }
                           deriving (Eq, Show, Ord)

newtype GitRepoName   = GitRepoName { unGitRepoName :: String }
                                    deriving (Eq, Show, Ord)

data RomefileEntry    = RomefileEntry { gitRepositoryName    :: GitRepoName
                                      , frameworkCommonNames :: [Framework]
                                      }
                                      deriving (Eq, Show)

data RomeFileParseResult = RomeFileParseResult { _cacheInfo            :: RomeCacheInfo
                                               , _repositoryMapEntries :: [RomefileEntry]
                                               , _ignoreMapEntries     :: [RomefileEntry]
                                               }
                                               deriving (Eq, Show)

frameworkName :: Lens' Framework String
frameworkName = lens _frameworkName (\framework newName -> framework { _frameworkName = newName })

frameworkType :: Lens' Framework FrameworkType
frameworkType = lens _frameworkType (\framework newType -> framework { _frameworkType = newType })

cacheInfo :: Lens' RomeFileParseResult RomeCacheInfo
cacheInfo = lens _cacheInfo (\parseResult n -> parseResult { _cacheInfo = n })

repositoryMapEntries :: Lens' RomeFileParseResult [RomefileEntry]
repositoryMapEntries = lens _repositoryMapEntries (\parseResult n -> parseResult { _repositoryMapEntries = n })

ignoreMapEntries :: Lens' RomeFileParseResult [RomefileEntry]
ignoreMapEntries = lens _ignoreMapEntries (\parseResult n -> parseResult { _ignoreMapEntries = n })



data RomeCacheInfo = RomeCacheInfo { _bucket        :: Maybe T.Text
                                   , _localCacheDir :: Maybe FilePath -- relative path
                                   }
                                   deriving (Eq, Show)

bucket :: Lens' RomeCacheInfo (Maybe T.Text)
bucket = lens _bucket (\cInfo n -> cInfo { _bucket = n })

localCacheDir :: Lens' RomeCacheInfo (Maybe FilePath)
localCacheDir = lens _localCacheDir (\cInfo n -> cInfo { _localCacheDir = n })

-- |The name of the Romefile
romefile :: String
romefile = "Romefile"

-- |The delimiter of the CACHE section a Romefile
cacheSectionDelimiter :: T.Text
cacheSectionDelimiter = "Cache"

-- |The S3-Bucket Key
s3BucketKey :: T.Text
s3BucketKey = "S3-Bucket"

-- |The local cache dir Key
localCacheDirKey :: T.Text
localCacheDirKey = "local"

-- |The delimier of the REPOSITORYMAP section
repositoryMapSectionDelimiter :: T.Text
repositoryMapSectionDelimiter = "RepositoryMap"

-- |The delimier of the IGNOREMAP section
ignoreMapSectionDelimiter :: T.Text
ignoreMapSectionDelimiter = "IgnoreMap"


parseRomefile :: T.Text -> Either String RomeFileParseResult
parseRomefile = toRomefile <=< INI.parseIni

toRomefile :: INI.Ini -> Either String RomeFileParseResult
toRomefile ini = do
  _bucket <- getBucket ini
  _localCacheDir <- getLocalCacheDir ini
  let _repositoryMapEntries = getRepositoryMapEntries ini
      _ignoreMapEntries = getIgnoreMapEntries ini
      _cacheInfo = RomeCacheInfo {..}
  return RomeFileParseResult {..}

getSection :: T.Text -> M.HashMap T.Text b -> Either String b
getSection key = maybe (Left err) Right . M.lookup key
  where
    err = "Could not find section: " <> show key

getBucket :: Ini -> Either String (Maybe T.Text)
getBucket (Ini ini) = M.lookup s3BucketKey <$> getSection cacheSectionDelimiter ini

getLocalCacheDir :: Ini -> Either String (Maybe FilePath)
getLocalCacheDir (Ini ini) =
  fmap T.unpack . M.lookup localCacheDirKey <$> getSection cacheSectionDelimiter ini

getRepositoryMapEntries :: Ini -> [RomefileEntry]
getRepositoryMapEntries = getRomefileEntries repositoryMapSectionDelimiter

getIgnoreMapEntries :: Ini -> [RomefileEntry]
getIgnoreMapEntries = getRomefileEntries ignoreMapSectionDelimiter

getRomefileEntries :: T.Text -> Ini -> [RomefileEntry]
getRomefileEntries sectionDelimiter (Ini ini) =
  fmap toEntry . M.toList . fromMaybe M.empty . M.lookup sectionDelimiter $ ini
  where
    toEntry :: (T.Text, T.Text) -> RomefileEntry
    toEntry _ = undefined
--    toEntry (repoName, frameworksAsStrings) =
--      RomefileEntry
--        (GitRepoName (T.unpack repoName))
--        (fmap (Framework . T.unpack . T.strip) (T.splitOn "," frameworksAsStrings))

    toFramework :: T.Text -> Framework
    toFramework t = case T.splitOn "/" t of
      [] -> undefined
      [fType, fName] -> Framework (T.unpack fName) (read $ T.unpack fType)



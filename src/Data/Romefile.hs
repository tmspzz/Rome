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
    , FrameworkType (..)
    , toFramework
    )
where

import           Control.Arrow        (left)
import           Control.Lens
import           Control.Monad.Except
import           Data.Char
import           Data.Either
import qualified Data.HashMap.Strict  as M
import           Data.Ini             as INI
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as T
import           Text.Read
import qualified Text.Read.Lex        as L



data FrameworkType = Dynamic
                   | Static deriving (Eq, Show, Ord)

instance Read FrameworkType where
  readPrec = parens $ do
    L.Ident s <- lexP
    case map toLower s of
      "dynamic" -> return Dynamic
      "static" -> return Static
      o -> fail $ "Could not parse '" ++ o ++ "' into a FrameworkType"

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

-- | Parses a Romefile
parseRomefile :: T.Text -> Either String RomeFileParseResult
parseRomefile = left T.unpack . toRomefile <=<  INI.parseIni

toRomefile :: INI.Ini -> Either T.Text RomeFileParseResult
toRomefile ini = do
  _bucket <- getBucket ini
  _localCacheDir <- getLocalCacheDir ini
  let _repositoryMapEntries = getRepositoryMapEntries ini
      _ignoreMapEntries = getIgnoreMapEntries ini
      _cacheInfo = RomeCacheInfo {..}
  RomeFileParseResult <$> Right _cacheInfo <*> _repositoryMapEntries <*> _ignoreMapEntries

getSection :: T.Text -> M.HashMap T.Text b -> Either T.Text b
getSection key = maybe (Left err) Right . M.lookup key
  where
    err = T.pack $ "Could not find section: " <> show key

getBucket :: Ini -> Either T.Text (Maybe T.Text)
getBucket (Ini ini) = M.lookup s3BucketKey <$> getSection cacheSectionDelimiter ini

getLocalCacheDir :: Ini -> Either T.Text (Maybe FilePath)
getLocalCacheDir (Ini ini) =
  fmap T.unpack . M.lookup localCacheDirKey <$> getSection cacheSectionDelimiter ini

getRepositoryMapEntries :: Ini -> Either T.Text [RomefileEntry]
getRepositoryMapEntries = getRomefileEntries repositoryMapSectionDelimiter

getIgnoreMapEntries :: Ini -> Either T.Text [RomefileEntry]
getIgnoreMapEntries = getRomefileEntries ignoreMapSectionDelimiter

getRomefileEntries :: T.Text -> Ini -> Either T.Text [RomefileEntry]
getRomefileEntries sectionDelimiter (Ini ini) =
  traverse toEntry . M.toList . fromMaybe M.empty . M.lookup sectionDelimiter $ ini

toEntry :: (T.Text, T.Text) -> Either T.Text RomefileEntry
toEntry (repoName, frameworksAsStrings) =
  let gitRepoName = GitRepoName $ T.unpack repoName
      eitherFrameworks = map (toFramework . T.strip) (T.splitOn "," frameworksAsStrings)
      (ls, rs) = partitionEithers eitherFrameworks
      errors =  T.intercalate "\n" ls in
        case ls of
          [] -> RomefileEntry <$> Right gitRepoName <*> Right rs
          _  -> Left errors

toFramework :: T.Text -> Either T.Text Framework
toFramework t = case T.splitOn "/" t of
  [] -> Left "Framework type and name are unespectedly empty"
  [fName] -> Right $ Framework (T.unpack fName) Dynamic
  [fType, fName] -> let upackedFtype = T.unpack fType
                        unpackedName = T.unpack fName
                        in
    left T.pack $
      Framework
        <$> Right (T.unpack fName)
        <*> (left (const (errorMessage unpackedName upackedFtype)) . readEither $ upackedFtype)
  (fType:fNameFragments) -> let upackedFtype = T.unpack fType
                                unpackedName = T.unpack $ T.intercalate "/" fNameFragments
                                in
    left T.pack $
      Framework
        <$> Right unpackedName
        <*> (left (const (errorMessage unpackedName upackedFtype)) . readEither . T.unpack $ fType)
  where
    errorMessage fType fName = "'"
      <> fType <>
      "' associated with '"
      <> fName <>
      "' is not a valid Framework type. Leave empty for 'dynamic' or use one of 'dynamic', 'static'."

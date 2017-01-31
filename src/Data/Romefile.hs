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
    , RomeCacheInfo (..)
    )
where

import           Data.Ini             as INI
import           Data.Ini.Utils       as INI
import           Data.HashMap.Strict  as M
import           Data.Monoid
import           Data.Maybe
import           Data.Text
import           Control.Monad.Except
import           Control.Monad.Trans
import           System.Directory
import           System.FilePath
import           System.Path.NameManip




newtype FrameworkName = FrameworkName { unFrameworkName :: String }
                        deriving (Eq, Show, Ord)

newtype GitRepoName   = GitRepoName { unGitRepoName :: String }
                        deriving (Eq, Show, Ord)

data RomefileEntry    = RomefileEntry { gitRepositoryName   :: GitRepoName
                                      , frameworkCommonNames :: [FrameworkName]
                                      }
                                      deriving (Show, Eq)

data RomeFileParseResult = RomeFileParseResult { cacheInfo :: RomeCacheInfo
                                               , repositoryMapEntries :: [RomefileEntry]
                                               , ignoreMapEntries :: [RomefileEntry]
                                               }

data RomeCacheInfo = RomeCacheInfo { _bucket :: Text
                                   , _localCacheDir :: Maybe FilePath
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

-- |The local cache dir Key
localCacheDirKey :: Text
localCacheDirKey = "local"

-- |The delimier of the REPOSITORYMAP section
repositoryMapSectionDelimiter :: Text
repositoryMapSectionDelimiter = "RepositoryMap"

-- |The delimier of the IGNOREMAP section
ignoreMapSectionDelimiter :: Text
ignoreMapSectionDelimiter = "IgnoreMap"


parseRomefile :: MonadIO m => FilePath -> ExceptT FilePath m RomeFileParseResult
parseRomefile f = do
  eitherIni <- liftIO $ INI.readIniFile f
  case eitherIni of
    Left iniError -> throwError iniError
    Right ini -> do
      _bucket <- withExceptT toErrorMessage $ getBucket ini
      maybeCacheDirAsText <- withExceptT toErrorMessage $ getLocalCacheDir ini
      _localCacheDir <- liftIO $ mapM absolutize (unpack <$> maybeCacheDirAsText)
      repositoryMapEntries <- getRepostiryMapEntries ini
      ignoreMapEntries <- getIgnoreMapEntries ini
      let cacheInfo = RomeCacheInfo {..}
      return RomeFileParseResult { .. }
  where
    toErrorMessage :: Text -> String
    toErrorMessage e = "Error while parsing " <> f <> ": " <> unpack e

getBucket :: MonadIO m => Ini -> ExceptT Text m Text
getBucket ini = requireKey s3BucketKey `inRequiredSection` cacheSectionDelimiter `fromIni''` ini

getLocalCacheDir :: MonadIO m => Ini -> ExceptT Text m (Maybe Text)
getLocalCacheDir ini = optionalKey localCacheDirKey `inRequiredSection` cacheSectionDelimiter `fromIni''` ini

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

-- | Take a path and makes it absolute resolving ../ and ~
-- See https://www.schoolofhaskell.com/user/dshevchenko/cookbook/transform-relative-path-to-an-absolute-path
absolutize :: FilePath -> IO FilePath
absolutize aPath
    | "~" `isPrefixOf` pack aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath
                             ++ Prelude.tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots

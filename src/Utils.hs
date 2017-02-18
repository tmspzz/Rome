{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module Utils where

import           Control.Lens         hiding (List)
import           Control.Monad.Trans  (MonadIO, lift, liftIO)
import           Data.Cartfile
import           Data.Function        (on)
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import           Data.Monoid
import           Data.Romefile
import qualified Data.Text            as T
import           Data.Time
import qualified Network.AWS          as AWS (Error, ErrorMessage (..),
                                              serviceMessage, _ServiceError)
import           System.FilePath
import           Types
import           Types.TargetPlatform



awsErrorToString :: AWS.Error -> String
awsErrorToString e = fromErrorMessage $ fromMaybe (AWS.ErrorMessage "Unexpected Error") maybeServiceError
  where
    maybeServiceError = view AWS.serviceMessage =<< (e ^? AWS._ServiceError)
    fromErrorMessage :: AWS.ErrorMessage -> String
    fromErrorMessage (AWS.ErrorMessage t) = T.unpack t

sayLn :: MonadIO m => String -> m ()
sayLn = liftIO . putStrLn

sayLnWithTime :: MonadIO m => String -> m ()
sayLnWithTime line = do
  time <- liftIO getZonedTime
  sayLn $ formatTime defaultTimeLocale "%T %F" time <> " - " <> line

roundBytesToMegabytes :: Integral a => a -> Double
roundBytesToMegabytes n = fromInteger (round (nInMB * (10^2))) / (10.0^^2)
  where
    nInMB = fromIntegral n / (1024*1024)

splitWithSeparator :: Char -> T.Text -> [T.Text]
splitWithSeparator a = T.split (== a)

appendFrameworkExtensionTo :: FrameworkName -> String
appendFrameworkExtensionTo (FrameworkName a) = a ++ ".framework"

frameworkArchiveName :: FrameworkName -> Version -> String
frameworkArchiveName f (Version v)  = appendFrameworkExtensionTo f ++ "-" ++ v ++ ".zip"

dSYMArchiveName :: FrameworkName -> Version -> String
dSYMArchiveName f (Version v) = appendFrameworkExtensionTo f ++ ".dSYM" ++ "-" ++ v ++ ".zip"

filterCartfileEntriesByGitRepoNames :: [GitRepoName] -> [CartfileEntry] -> [CartfileEntry]
filterCartfileEntriesByGitRepoNames repoNames cartfileEntries = [c | c <- cartfileEntries, gitRepoNameFromCartfileEntry c `elem` repoNames]

gitRepoNameFromCartfileEntry :: CartfileEntry -> GitRepoName
gitRepoNameFromCartfileEntry (CartfileEntry GitHub (Location l) _) = GitRepoName . T.unpack . last . splitWithSeparator '/' . T.pack $ l
gitRepoNameFromCartfileEntry (CartfileEntry Git (Location l) _) = GitRepoName . T.unpack . T.replace ".git" "" . last . splitWithSeparator '/' . T.pack $ l

filterByNameEqualTo :: [FrameworkVersion] -> FrameworkName -> [FrameworkVersion]
filterByNameEqualTo fs s = filter (\(FrameworkVersion name version) -> name == s) fs

filterOutFrameworkNamesAndVersionsIfNotIn :: [FrameworkVersion] -> [FrameworkName] -> [FrameworkVersion]
filterOutFrameworkNamesAndVersionsIfNotIn favs fns = [fv |  fv <- favs,  _frameworkName fv `notElem` fns]

restrictRepositoryMapToGitRepoName:: RepositoryMap -> GitRepoName -> RepositoryMap
restrictRepositoryMapToGitRepoName repoMap repoName = maybe M.empty (M.singleton repoName) $ repoName `M.lookup` repoMap

remoteFrameworkPath :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> Version -> String
remoteFrameworkPath p r f v = remoteCacheDirectory p r f ++ frameworkArchiveName f v

remoteDsymPath :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> Version -> String
remoteDsymPath p r f v = remoteCacheDirectory p r f ++ dSYMArchiveName f v

remoteCacheDirectory :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> String
remoteCacheDirectory p r f = repoName </> show p ++ "/"
  where
    repoName = unGitRepoName $ repoNameForFrameworkName r f

toRepositoryMap :: [RomefileEntry] -> RepositoryMap
toRepositoryMap = M.fromList . map romeFileEntryToTuple

toInvertedRepositoryMap :: [RomefileEntry] -> InvertedRepositoryMap
toInvertedRepositoryMap = M.fromList . concatMap romeFileEntryToListOfTuples
  where listify (fs, g) = map (\f -> (f,g)) fs
        flipTuple = uncurry (flip (,))
        romeFileEntryToListOfTuples = listify . flipTuple . romeFileEntryToTuple

romeFileEntryToTuple :: RomefileEntry -> (GitRepoName, [FrameworkName])
romeFileEntryToTuple RomefileEntry {..} = (gitRepositoryName, frameworkCommonNames)

repoNameForFrameworkName :: InvertedRepositoryMap -> FrameworkName -> GitRepoName
repoNameForFrameworkName reverseRomeMap frameworkName = fromMaybe (GitRepoName . unFrameworkName $ frameworkName) (M.lookup frameworkName reverseRomeMap)

formattedPlatformAvailability :: PlatformAvailability -> String
formattedPlatformAvailability p = availabilityPrefix p ++ platformName p
  where
    availabilityPrefix (PlatformAvailability _ True) = "+"
    availabilityPrefix (PlatformAvailability _ False) = "-"
    platformName = show . _availabilityPlatform

deriveFrameworkNamesAndVersion :: RepositoryMap -> [CartfileEntry] -> [FrameworkVersion]
deriveFrameworkNamesAndVersion romeMap = concatMap (deriveFrameworkNameAndVersion romeMap)

deriveFrameworkNameAndVersion ::  RepositoryMap -> CartfileEntry -> [FrameworkVersion]
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry GitHub (Location l) v) = map (`FrameworkVersion` v) $
  fromMaybe [FrameworkName gitHubRepositoryName] (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
  where
    gitHubRepositoryName = unGitRepoName $ gitRepoNameFromCartfileEntry cfe
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry Git (Location l) v)    = map (`FrameworkVersion` v) $
  fromMaybe [FrameworkName gitRepositoryName] (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
  where
    gitRepositoryName = unGitRepoName $ gitRepoNameFromCartfileEntry cfe

constructFrameworksAndVersionsFrom :: [CartfileEntry] -> RepositoryMap -> [FrameworkVersion]
constructFrameworksAndVersionsFrom cartfileEntries repositoryMap = deriveFrameworkNamesAndVersion repositoryMap cartfileEntries

filterRepoMapByGitRepoNames :: RepositoryMap -> [GitRepoName] -> RepositoryMap
filterRepoMapByGitRepoNames repoMap gitRepoNames = M.unions $ map (restrictRepositoryMapToGitRepoName repoMap) gitRepoNames

getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities :: InvertedRepositoryMap -> [FrameworkAvailability] -> [GitRepoAvailability]
getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRomeMap = concatMap mergeRepoAvailabilities . groupAvailabilities . getGitRepoAvalabilities
  where
    getGitRepoAvalabilities :: [FrameworkAvailability] -> [GitRepoAvailability]
    getGitRepoAvalabilities = fmap getGitRepoAvailabilityFromFrameworkAvailability

    getGitRepoAvailabilityFromFrameworkAvailability :: FrameworkAvailability -> GitRepoAvailability
    getGitRepoAvailabilityFromFrameworkAvailability (FrameworkAvailability (FrameworkVersion fwn v) availabilities) = GitRepoAvailability (repoNameForFrameworkName reverseRomeMap fwn) v availabilities

    groupAvailabilities :: [GitRepoAvailability] -> [[GitRepoAvailability]]
    groupAvailabilities = groupBy ((==) `on` _availabilityRepo) . sortBy (compare `on` _availabilityRepo)

mergeRepoAvailabilities :: [GitRepoAvailability] -> [GitRepoAvailability]
mergeRepoAvailabilities repoAvailabilities@(x:xs) = [x { _repoPlatformAvailabilities = platformAvailabilities }]
  where
    sortAndGroupPlatformAvailabilities = groupBy ((==) `on` _availabilityPlatform) . sortBy (compare `on` _availabilityPlatform)
    groupedPlatformAvailabilities = sortAndGroupPlatformAvailabilities (repoAvailabilities >>= _repoPlatformAvailabilities)
    bothAvailable p p' = p { _isAvailable = _isAvailable p && _isAvailable p' }
    platformAvailabilities = fmap (foldl1 bothAvailable) groupedPlatformAvailabilities

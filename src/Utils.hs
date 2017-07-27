{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Utils where

import           Configuration                (carthageBuildDirectoryForPlatform)
import           Control.Arrow                (left)
import           Control.Exception            as E (try)
import           Control.Lens                 hiding (List)
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import           Data.Char                    (isNumber)
import           Data.Function                (on)
import           Data.List
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromMaybe, fromJust)
import           Data.Monoid
import           Data.Romefile
import qualified Data.Text                    as T
import           Data.Time
import qualified Network.AWS                  as AWS (Error, ErrorMessage (..),
                                                      serviceMessage,
                                                      _ServiceError)
import           Network.HTTP.Conduit         as HTTP
import           Network.HTTP.Types.Header    as HTTP (hUserAgent)
import           Numeric                      (showFFloat)
import           System.Directory             (getHomeDirectory)
import           System.FilePath              ((</>), normalise, addTrailingPathSeparator)
import           System.Path.NameManip        (absolute_path, guess_dotdot)
import           Text.Read                    (readMaybe)
import           Types


-- | Pretty print a `RomeVersion`
romeVersionToString :: RomeVersion -> String
romeVersionToString (major, minor, patch, build) = show major <> "." <> show minor <> "." <> show patch <> "." <> show build



-- | Check if the given `RomeVersion` is the latest version compared to GitHub releases
checkIfRomeLatestVersionIs :: MonadIO m => RomeVersion -> ExceptT String m (Bool, RomeVersion)
checkIfRomeLatestVersionIs currentRomeVersion = do
  req <- liftIO $ HTTP.parseRequest "https://api.github.com/repos/blender/Rome/releases/latest"

  let headers = HTTP.requestHeaders req <> [(HTTP.hUserAgent, userAgent)]
  let req' = req { HTTP.responseTimeout = timeout, HTTP.requestHeaders = headers }

  manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings

  eitherBody :: Either HTTP.HttpException LBS.ByteString <- liftIO $ E.try (HTTP.responseBody <$> HTTP.httpLbs req' manager)

  let eitherTagName :: Either String String = left show eitherBody >>= eitherDecode >>= \d -> flip parseEither d $ \obj -> obj .: "tag_name"

  either throwError return $ (\tagVersion -> (currentRomeVersion >= tagVersion, tagVersion)) . stringToVersionTuple <$> eitherTagName

    where
      stringToVersionTuple = versionTupleOrZeros . map (fromMaybe 0 . readMaybe . T.unpack) . take 4 . splitWithSeparator '.' . T.pack . dropWhile (not . isNumber)
      versionTupleOrZeros a = (fromMaybe 0 (a !!? 0), fromMaybe 0 (a !!? 1), fromMaybe 0 (a !!? 2), fromMaybe 0 (a !!? 3))

      timeout = responseTimeoutMicro 1000000 -- 1 second
      userAgent = BS.pack $ "Rome/" <> romeVersionToString currentRomeVersion



-- | Gets `Just` the element at the specified index or `Nothing`
(!!?) :: [a] -> Int -> Maybe a
(!!?) a i | i < length a = Just (a !! i)
          | otherwise    = Nothing



-- | Turns an `AWS.Error` to `String` or defaults to "Unexpected Error".
awsErrorToString :: AWS.Error -> String
awsErrorToString e = fromErrorMessage $ fromMaybe (AWS.ErrorMessage "Unexpected Error") maybeServiceError
  where
    maybeServiceError = view AWS.serviceMessage =<< (e ^? AWS._ServiceError)
    fromErrorMessage :: AWS.ErrorMessage -> String
    fromErrorMessage (AWS.ErrorMessage t) = T.unpack t



-- | Prints a `String` doing the lifting for you.
sayLn :: MonadIO m => String -> m ()
sayLn = liftIO . putStrLn



-- | Prints a message adding a time stamp before it.
sayLnWithTime :: MonadIO m => String -> m ()
sayLnWithTime line = do
  time <- liftIO getZonedTime
  sayLn $ formatTime defaultTimeLocale "%T %F" time <> " - " <> line



-- | Given a number n representing bytes, shows it in MB, rounded to 2 decimal places.
showInMegabytes :: Integral n => n -> String
showInMegabytes n = showFFloat (Just 2) nInMB " MB"
  where
    nInMB :: Double
    nInMB = fromIntegral n / (1024*1024)



-- | Splits a `T.Text` in substrings at every occurence of a given character.
splitWithSeparator :: Char -> T.Text -> [T.Text]
splitWithSeparator a = T.split (== a)



-- | Appends the string ".framework" to a `FrameworkName`.
appendFrameworkExtensionTo :: FrameworkName -> String
appendFrameworkExtensionTo (FrameworkName a) = a ++ ".framework"



-- | Given a `FrameworkName` and a `Version` produces a name for a Zip archive.
frameworkArchiveName :: FrameworkName -> Version -> String
frameworkArchiveName f (Version v)  = appendFrameworkExtensionTo f ++ "-" ++ v ++ ".zip"



-- | Given a `FrameworkName` and a `Version` produces a name
-- | for a dSYM Zip archive.
dSYMArchiveName :: FrameworkName -> Version -> String
dSYMArchiveName f (Version v) = appendFrameworkExtensionTo f ++ ".dSYM" ++ "-" ++ v ++ ".zip"



-- | Given a list of `CartfileEntry`s  and a list of `GitRepoName`s
-- | produces a list of `CartfileEntry`s filtered by `GitRepoName`s
filterCartfileEntriesByGitRepoNames :: [GitRepoName] -> [CartfileEntry] -> [CartfileEntry]
filterCartfileEntriesByGitRepoNames repoNames cartfileEntries = [c | c <- cartfileEntries, gitRepoNameFromCartfileEntry c `elem` repoNames]



-- | Given a `CartfileEntry` produces a `GitRepoName`.
--
-- >>> gitRepoNameFromCartfileEntry $ CartfileEntry Git (Location "https://repo.acme.inc/acmeFramework.git") (Version "1.2.3")
-- GitRepoName {unGitRepoName = "acmeFramework"}
--
-- >>> gitRepoNameFromCartfileEntry $ CartfileEntry GitHub (Location "acme/acmeFramework") (Version "1.2.3")
-- GitRepoName {unGitRepoName = "acmeFramework"}
gitRepoNameFromCartfileEntry :: CartfileEntry -> GitRepoName
gitRepoNameFromCartfileEntry (CartfileEntry GitHub (Location l) _) = GitRepoName . T.unpack . last . splitWithSeparator '/' . T.pack $ l
gitRepoNameFromCartfileEntry (CartfileEntry Git (Location l) _) = GitRepoName . T.unpack . T.replace ".git" "" . last . splitWithSeparator '/' . T.pack $ l



-- | Given a lsit of `FrameworkVersion` and a `FrameworkName` returns
-- | a list for `FrameworkVersion` elements matching `FrameworkName`.
filterByNameEqualTo :: [FrameworkVersion] -> FrameworkName -> [FrameworkVersion]
filterByNameEqualTo fs s = filter (\(FrameworkVersion name _) -> name == s) fs



-- | Given a list of `FrameworkVersion` and a list of `FrameworkName`
-- | filters out of the list of `FrameworkVersion` elements that don't apper
-- | in the list of `FrameworkName`.
filterOutFrameworkNamesAndVersionsIfNotIn :: [FrameworkVersion] -> [FrameworkName] -> [FrameworkVersion]
filterOutFrameworkNamesAndVersionsIfNotIn favs fns = [fv |  fv <- favs,  _frameworkName fv `notElem` fns]



-- | Given a `RepositoryMap` and a `GitRepoName` returns a `RepositoryMap`
-- | with that one `GitRepoName` or an empty `RepositoryMap`.
restrictRepositoryMapToGitRepoName:: RepositoryMap -> GitRepoName -> RepositoryMap
restrictRepositoryMapToGitRepoName repoMap repoName = maybe M.empty (M.singleton repoName) $ repoName `M.lookup` repoMap



-- | Builds a string representing the remote path to framework zip archive.
remoteFrameworkPath :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> Version -> String
remoteFrameworkPath p r f v = remoteCacheDirectory p r f ++ frameworkArchiveName f v



-- | Builds a `String` representing the remote path to dSYM zip archive
remoteDsymPath :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> Version -> String
remoteDsymPath p r f v = remoteCacheDirectory p r f ++ dSYMArchiveName f v



-- | Builds a `String` representing the name of the remote cache directory for a
-- | given conbination of `TargetPlatform` and `FrameworkName` based on an
-- | `InvertedRepositoryMap`.
remoteCacheDirectory :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> String
remoteCacheDirectory p r f = repoName </> show p ++ "/"
  where
    repoName = unGitRepoName $ repoNameForFrameworkName r f



-- | Builds a `String` representing the name of the VersionFile for a given
-- | `GitRepoNameAndVersion`
remoteVersionFilePath :: GitRepoNameAndVersion -> String
remoteVersionFilePath (gitRepoName, version) = unGitRepoName gitRepoName </> versionFileNameForGitRepoNameVersioned gitRepoName version



-- | Builds a `String` representing the path to the Carthage build directory for
-- | a combination of `TargetPlatform` and `FrameworkName` representing
-- | the path to the framework's bundle
frameworkBuildBundleForPlatform :: TargetPlatform -> FrameworkName -> String
frameworkBuildBundleForPlatform p f = carthageBuildDirectoryForPlatform p </> appendFrameworkExtensionTo f



-- | Constructs a `RepositoryMap` from a list of `RomefileEntry`s.
-- | The keys are `GitRepoName`s.
toRepositoryMap :: [RomefileEntry] -> RepositoryMap
toRepositoryMap = M.fromList . map romeFileEntryToTuple



-- | Constructs an `InvertedRepositoryMap` from a list of `RomefileEntry`s.
-- | The keys are `FrameworkName`s.
toInvertedRepositoryMap :: [RomefileEntry] -> InvertedRepositoryMap
toInvertedRepositoryMap = M.fromList . concatMap romeFileEntryToListOfTuples
  where listify (fs, g) = map (\f -> (f,g)) fs
        flipTuple = uncurry (flip (,))
        romeFileEntryToListOfTuples = listify . flipTuple . romeFileEntryToTuple



-- | Creates a tuple out of a `RomefileEntry`.
romeFileEntryToTuple :: RomefileEntry -> (GitRepoName, [FrameworkName])
romeFileEntryToTuple RomefileEntry {..} = (gitRepositoryName, frameworkCommonNames)



-- | Performs a lookup in an `InvertedRepositoryMap` for a certain `FrameworkName`.
-- | Creates a `GitRepoName` from just the `frameworkName` of a `FrameworkName`
-- | in case the lookup fails.
repoNameForFrameworkName :: InvertedRepositoryMap -> FrameworkName -> GitRepoName
repoNameForFrameworkName reverseRomeMap frameworkName = fromMaybe (GitRepoName . unFrameworkName $ frameworkName) (M.lookup frameworkName reverseRomeMap)



-- | Given an `InvertedRepositoryMap` and a list of  `FrameworkVersion` produces
-- | a list of __unique__ `GitRepoName`s
repoNamesForFrameworkVersion :: InvertedRepositoryMap -> [FrameworkVersion] -> [GitRepoName]
repoNamesForFrameworkVersion reverseRomeMap = nub . map (repoNameForFrameworkName reverseRomeMap . _frameworkName)



-- | Given an `InvertedRepositoryMap` and a list of  `FrameworkVersion` produces
-- | a list of __unique__ `GitRepoNameAndVersion`s
repoNamesAndVersionForFrameworkVersions :: InvertedRepositoryMap -> [FrameworkVersion] -> [GitRepoNameAndVersion]
repoNamesAndVersionForFrameworkVersions reverseRomeMap frameworkNames = nub $
  zip (map (repoNameForFrameworkName reverseRomeMap . _frameworkName) frameworkNames)
      (map _frameworkVersion frameworkNames)



-- | Given a `GitRepoName` produces the appropriate file name for the corresponding
-- | Carthage VersionFile
versionFileNameForGitRepoName :: GitRepoName -> String
versionFileNameForGitRepoName grn = "." <> unGitRepoName grn <> ".version"



-- | Given a `GitRepoName` produces the appropriate file name for the corresponding
-- | Carthage VersionFile with appenended `Version` information
versionFileNameForGitRepoNameVersioned :: GitRepoName -> Version -> String
versionFileNameForGitRepoNameVersioned grn version = versionFileNameForGitRepoName grn <> "-" <> unVersion version



-- | Given a `PlatformAvailability` produces a human readable `String`
-- | representing the availability.
--
-- >>> formattedPlatformAvailability $ PlatformAvailability IOS True
-- "+iOS"
--
-- >>> formattedPlatformAvailability $ PlatformAvailability MacOS False
-- "-macOS"
formattedPlatformAvailability :: PlatformAvailability -> String
formattedPlatformAvailability p = availabilityPrefix p ++ platformName p
  where
    availabilityPrefix (PlatformAvailability _ True) = "+"
    availabilityPrefix (PlatformAvailability _ False) = "-"
    platformName = show . _availabilityPlatform



-- | Given a `RepositoryMap` and a list of `CartfileEntry` creates a list of
-- | `FrameworkVersion`s. See `deriveFrameworkNameAndVersion` for details.
deriveFrameworkNamesAndVersion :: RepositoryMap -> [CartfileEntry] -> [FrameworkVersion]
deriveFrameworkNamesAndVersion romeMap = concatMap (deriveFrameworkNameAndVersion romeMap)



-- | Given a `RepositoryMap` and a `CartfileEntry` creates a list of
-- | `FrameworkVersion` by attaching the `Version` information from the
-- | `FrameworkName` in the `CartfileEntry`.
deriveFrameworkNameAndVersion ::  RepositoryMap -> CartfileEntry -> [FrameworkVersion]
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry GitHub (Location _) v) = map (`FrameworkVersion` v) $
  fromMaybe [FrameworkName gitHubRepositoryName] (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
  where
    gitHubRepositoryName = unGitRepoName $ gitRepoNameFromCartfileEntry cfe
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry Git (Location _) v)    = map (`FrameworkVersion` v) $
  fromMaybe [FrameworkName gitRepositoryName] (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
  where
    gitRepositoryName = unGitRepoName $ gitRepoNameFromCartfileEntry cfe



-- | Given a `RepositoryMap` and a list of `GitRepoName`s produces another
-- | `RepositoryMap` containing only those `GitRepoName`s.
filterRepoMapByGitRepoNames :: RepositoryMap -> [GitRepoName] -> RepositoryMap
filterRepoMapByGitRepoNames repoMap gitRepoNames = M.unions $ map (restrictRepositoryMapToGitRepoName repoMap) gitRepoNames



-- | Given an `InvertedRepositoryMap` and a list of `FrameworkAvailability`s
-- | produces the corresponding list of `GitRepoAvailability`s.
getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities :: InvertedRepositoryMap -> [FrameworkAvailability] -> [GitRepoAvailability]
getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRomeMap = concatMap mergeRepoAvailabilities . groupAvailabilities . getGitRepoAvalabilities
  where
    getGitRepoAvalabilities :: [FrameworkAvailability] -> [GitRepoAvailability]
    getGitRepoAvalabilities = fmap getGitRepoAvailabilityFromFrameworkAvailability

    getGitRepoAvailabilityFromFrameworkAvailability :: FrameworkAvailability -> GitRepoAvailability
    getGitRepoAvailabilityFromFrameworkAvailability (FrameworkAvailability (FrameworkVersion fwn v) availabilities) = GitRepoAvailability (repoNameForFrameworkName reverseRomeMap fwn) v availabilities

    groupAvailabilities :: [GitRepoAvailability] -> [[GitRepoAvailability]]
    groupAvailabilities = groupBy ((==) `on` _availabilityRepo) . sortBy (compare `on` _availabilityRepo)

    -- | Given a list of `GitRepoAvailability`s produces a singleton list of
    -- | `GitRepoAvailability`s containing all `PlatformAvailability`s of the
    -- | original list.
    --
    -- >>> let g1 = GitRepoAvailability (GitRepoName "Alamofire") (Version "1") [PlatformAvailability IOS True]
    -- >>> let g2 = GitRepoAvailability (GitRepoName "Alamofire") (Version "2") [PlatformAvailability MacOS True]
    -- >>> let g3 = GitRepoAvailability (GitRepoName "CoreStore") (Version "3") [PlatformAvailability TVOS True]
    -- >>> mergeRepoAvailabilities [g1, g2, g3]
    -- [GitRepoAvailability {_availabilityRepo = GitRepoName {unGitRepoName = "Alamofire"}
    --                      , _availabilityVersion = Version {unVersion = "1"}
    --                      , _repoPlatformAvailabilities = [PlatformAvailability {_availabilityPlatform = iOS, _isAvailable = True}
    --                                                      ,PlatformAvailability {_availabilityPlatform = macOS, _isAvailable = True}
    --                                                      ,PlatformAvailability {_availabilityPlatform = tvOS, _isAvailable = True}]
    --                      }
    -- ]
    mergeRepoAvailabilities :: [GitRepoAvailability] -> [GitRepoAvailability]
    mergeRepoAvailabilities [] = []
    mergeRepoAvailabilities repoAvailabilities@(x:_) = [x { _repoPlatformAvailabilities = platformAvailabilities }]
      where
        groupedPlatformAvailabilities :: [[PlatformAvailability]]
        groupedPlatformAvailabilities = sortAndGroupPlatformAvailabilities (repoAvailabilities >>= _repoPlatformAvailabilities)

        bothAvailable :: PlatformAvailability -> PlatformAvailability -> PlatformAvailability
        bothAvailable p p' = p { _isAvailable = _isAvailable p && _isAvailable p' }

        platformAvailabilities :: [PlatformAvailability]
        platformAvailabilities = fmap (foldl1 bothAvailable) groupedPlatformAvailabilities

        sortAndGroupPlatformAvailabilities :: [PlatformAvailability] -> [[PlatformAvailability]]
        sortAndGroupPlatformAvailabilities = groupBy ((==) `on` _availabilityPlatform) . sortBy (compare `on` _availabilityPlatform)


--- | Take a path and makes it absolute resolving ../ and ~
--- See https://www.schoolofhaskell.com/user/dshevchenko/cookbook/transform-relative-path-to-an-absolute-path
absolutizePath :: FilePath -> IO FilePath
absolutizePath aPath
    | "~" `T.isPrefixOf` T.pack aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath
                             ++ Prelude.tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots



redControlSequence :: String
redControlSequence = "\ESC[0;31m"

greenControlSequence :: String
greenControlSequence = "\ESC[0;32m"

noColorControlSequence :: String
noColorControlSequence = "\ESC[0m"

third :: (a, b, c) -> c
third (_, _, c) = c

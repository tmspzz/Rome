{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Utils where

import qualified Codec.Archive.Zip             as Zip
import           Configuration                  ( carthageArtifactsBuildDirectoryForPlatform )
import           Control.Arrow                  ( left )
import           Control.Exception             as E
                                                ( try )
import           Control.Lens            hiding ( List )
import           Control.Monad.Except
import           Control.Monad.Trans.Resource   ( MonadUnliftIO
                                                , runResourceT
                                                )
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import           Data.Char                      ( isNumber )
import qualified Data.Conduit                  as C
                                                ( runConduit
                                                , (.|)
                                                )
import qualified Data.Conduit.Binary           as C
                                                ( sinkFile
                                                , sourceLbs
                                                )
import           Data.Function                  ( on )
import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Data.Romefile
import qualified Data.Text                     as T
import           Data.Text.Encoding
import qualified Data.Text.IO                  as T
import           Data.Time
import qualified Network.AWS                   as AWS
                                                ( Error
                                                , ErrorMessage(..)
                                                , serviceMessage
                                                , _ServiceError
                                                )
import qualified Network.AWS.Data.Text         as AWS
                                                ( showText )

import           Network.HTTP.Conduit          as HTTP
import           Network.HTTP.Types.Header     as HTTP
                                                ( hUserAgent )
import           Numeric                        ( showFFloat )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , getHomeDirectory
                                                , removeFile
                                                , canonicalizePath
                                                )
import           System.FilePath                ( addTrailingPathSeparator
                                                , dropFileName
                                                , normalise
                                                , (</>)
                                                )
import           Text.Read                      ( readMaybe )
import qualified Turtle
import           Types
import           Types.Commands
import           Xcode.DWARF                    ( DwarfUUID
                                                , bcsymbolmapNameFrom
                                                )



-- | Pretty print a `RomeVersion`
romeVersionToString :: RomeVersion -> String
romeVersionToString (major, minor, patch, build) =
  show major <> "." <> show minor <> "." <> show patch <> "." <> show build

-- | Check if the given `RomeVersion` is the latest version compared to GitHub releases
checkIfRomeLatestVersionIs :: MonadIO m => RomeVersion -> ExceptT String m (Bool, RomeVersion)
checkIfRomeLatestVersionIs currentRomeVersion = do
  req <- liftIO $ HTTP.parseRequest "https://api.github.com/repos/blender/Rome/releases/latest"

  let headers = HTTP.requestHeaders req <> [(HTTP.hUserAgent, userAgent)]
  let req'    = req { HTTP.responseTimeout = timeout, HTTP.requestHeaders = headers }

  manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings

  eitherBody :: Either HTTP.HttpException LBS.ByteString <- liftIO
    $ E.try (HTTP.responseBody <$> HTTP.httpLbs req' manager)

  let eitherTagName :: Either String String =
        left show eitherBody >>= eitherDecode >>= \d -> flip parseEither d $ \obj -> obj .: "tag_name"

  either throwError return
    $   (\tagVersion -> (currentRomeVersion >= tagVersion, tagVersion))
    .   stringToVersionTuple
    <$> eitherTagName
 where
  stringToVersionTuple =
    versionTupleOrZeros
      . map (fromMaybe 0 . readMaybe . T.unpack)
      . take 4
      . splitWithSeparator '.'
      . T.pack
      . dropWhile (not . isNumber)
  versionTupleOrZeros a = (fromMaybe 0 (a !!? 0), fromMaybe 0 (a !!? 1), fromMaybe 0 (a !!? 2), fromMaybe 0 (a !!? 3))

  timeout   = responseTimeoutMicro 1000000 -- 1 second
  userAgent = BS.pack $ "Rome/" <> romeVersionToString currentRomeVersion



-- | Gets `Just` the element at the specified index or `Nothing`
(!!?) :: [a] -> Int -> Maybe a
(!!?) a i | i < length a = Just (a !! i)
          | otherwise    = Nothing



-- | Turns an `AWS.Error` to `String` or defaults to "Unexpected Error".
awsErrorToString :: AWS.Error -> Bool -> String
awsErrorToString e verbose = if verbose
  then show e
  else AWS.showText $ fromMaybe (AWS.ErrorMessage "Unexpected Error") maybeServiceError
  where maybeServiceError = view AWS.serviceMessage =<< (e ^? AWS._ServiceError)



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
  nInMB = fromIntegral n / (1024 * 1024)



-- | Splits a `T.Text` in substrings at every occurence of a given character.
splitWithSeparator :: Char -> T.Text -> [T.Text]
splitWithSeparator a = T.split (== a)



-- | Appends the string ".framework" to a `Framework`'s name.
appendFrameworkExtensionTo :: Framework -> String
appendFrameworkExtensionTo (Framework a _ _) = a ++ ".framework"



-- | Appends the string ".xcframework" to a `XCFramework`'s name.
appendXcFrameworkExtensionTo :: Framework -> String
appendXcFrameworkExtensionTo (Framework a _ _) = a ++ ".xcframework"



-- | Given a `Framework` and a `Version` produces a name for a Zip archive.
frameworkArchiveName :: Framework -> Version -> Bool -> String
frameworkArchiveName f@(Framework _ Dynamic _) (Version v) x =
  if x then appendXcFrameworkExtensionTo f ++ "-" ++ v ++ ".zip"
  else appendFrameworkExtensionTo f ++ "-" ++ v ++ ".zip"
frameworkArchiveName f@(Framework _ Static _) (Version v) x =
  if x then appendXcFrameworkExtensionTo f ++ "-" ++ "static" ++ "-" ++ v ++ ".zip"
  else appendFrameworkExtensionTo f ++ "-" ++ "static" ++ "-" ++ v ++ ".zip"



-- | Given a `Framework` and a `Version` produces a name
-- | for a dSYM Zip archive.
dSYMArchiveName :: Framework -> Version -> String
dSYMArchiveName f@(Framework _ Dynamic _) (Version v) = appendFrameworkExtensionTo f ++ ".dSYM" ++ "-" ++ v ++ ".zip"
dSYMArchiveName f@(Framework _ Static _) (Version v) =
  appendFrameworkExtensionTo f ++ ".dSYM" ++ "-" ++ "static" ++ "-" ++ v ++ ".zip"



-- | Given a `DwarfUUID` and a `Version` produces a name
-- | for a bcsymbolmap Zip archive.
bcsymbolmapArchiveName :: DwarfUUID -> Version -> String
bcsymbolmapArchiveName d (Version v) = bcsymbolmapNameFrom d ++ "-" ++ v ++ ".zip"



-- | Given a list of `CartfileEntry`s  and a list of `ProjectName`s
-- | produces a list of `CartfileEntry`s filtered by `ProjectName`s
filterCartfileEntriesByGitRepoNames :: [ProjectName] -> [CartfileEntry] -> [CartfileEntry]
filterCartfileEntriesByGitRepoNames repoNames cartfileEntries =
  [ c | c <- cartfileEntries, gitRepoNameFromCartfileEntry c `elem` repoNames ]



-- | Given a `CartfileEntry` produces a `ProjectName`.
--
-- >>> gitRepoNameFromCartfileEntry $ CartfileEntry Git (Location "https://repo.acme.inc/acmeFramework.git") (Version "1.2.3")
-- ProjectName {unProjectName = "acmeFramework"}
--
-- >>> gitRepoNameFromCartfileEntry $ CartfileEntry GitHub (Location "acme/acmeFramework") (Version "1.2.3")
-- ProjectName {unProjectName = "acmeFramework"}
gitRepoNameFromCartfileEntry :: CartfileEntry -> ProjectName
gitRepoNameFromCartfileEntry (CartfileEntry GitHub (Location l) _) =
  ProjectName . T.unpack . last . splitWithSeparator '/' . T.pack $ l
gitRepoNameFromCartfileEntry (CartfileEntry Git (Location l) _) =
  ProjectName . T.unpack . T.replace ".git" "" . last . splitWithSeparator '/' . T.pack $ l
gitRepoNameFromCartfileEntry (CartfileEntry Binary (Location l) _) =
  ProjectName . T.unpack . head . T.splitOn ".json" . last . splitWithSeparator '/' . T.pack $ l



-- | Given a list of `FrameworkVersion` and a `Framework` returns
-- | a list for `FrameworkVersion` elements matching `Framework`.
filterByFrameworkEqualTo :: [FrameworkVersion] -> Framework -> [FrameworkVersion]
filterByFrameworkEqualTo versions f = [ ver | ver <- versions, _framework ver == f ]



-- | Given a list of `FrameworkVersion` and a list of `Framework`
-- | filters out of the list of `FrameworkVersion` elements that don't apper
-- | in the list of `Framework`.
filterOutFrameworksAndVersionsIfNotIn :: [FrameworkVersion] -> [Framework] -> [FrameworkVersion]
filterOutFrameworksAndVersionsIfNotIn versions frameworks = do
  ver@(FrameworkVersion f@(Framework n t _) v) <- versions -- For each version
  let filteredFrameworks = (\(Framework nF tF _) -> nF == n && tF == t) `filter` frameworks -- filter the frameworks to exclude based on name and type, not on the platforms
  if null filteredFrameworks -- If none match
    then return ver -- don't filter this FrameworkVersion out
    else do  -- if there there are matches
      let filteredFrameworks2 = f `removePlatformsIn` nub (concatMap _frameworkPlatforms filteredFrameworks)
      guard (not . null $ _frameworkPlatforms filteredFrameworks2) -- if the entry completely filters out the FrameworkVersion then remove it
      return $ FrameworkVersion filteredFrameworks2 v -- if it doesn't, then remove from f the platforms that appear in the filter above.
 where
  removePlatformsIn :: Framework -> [TargetPlatform] -> Framework
  removePlatformsIn (Framework n t ps) rPs = Framework n t [ p | p <- ps, p `notElem` rPs ]



removeIntersectingPlatforms :: [Framework] -> [Framework] -> [Framework]
removeIntersectingPlatforms lhs rhs = do
  f <- lhs
  return $ foldl removeIntersectingPlatforms' f rhs
 where
  -- | Given a `Framework` and a list of `TargetPlatform`
  -- | remove the overlapping platforms
  removeIntersectingPlatforms' :: Framework -> Framework -> Framework
  removeIntersectingPlatforms' f1@(Framework n t ps) (Framework n2 t2 ps2)
    | n == n2 && t == t2 && (not . null) (ps `intersect` ps2) = Framework n t [ p | p <- ps, p `notElem` ps2 ]
    | otherwise = f1



-- | Given a `RepositoryMap` and a `ProjectName` returns a `RepositoryMap`
-- | with that one `ProjectName` or an empty `RepositoryMap`.
restrictRepositoryMapToGitRepoName :: RepositoryMap -> ProjectName -> RepositoryMap
restrictRepositoryMapToGitRepoName repoMap repoName =
  maybe M.empty (M.singleton repoName) $ repoName `M.lookup` repoMap



-- | Given two lists of `RomefileEntry`, adjust the entries in one list
-- | according to entries in the other list. Specifically remove the platforms that
-- | are common in both entries. If the resulting platforms are empty, remove the entry.
filterRomeFileEntriesByPlatforms :: [RomefileEntry] -> [RomefileEntry] -> [RomefileEntry]
filterRomeFileEntriesByPlatforms lhs rhs = (uncurry RomefileEntry <$>) . M.toList $ lhsMap `purgingPlatformsIn` rhsMap
 where
  purgingPlatformsIn = M.differenceWith purge
  purge a b =
    let filteredEntries = (\(Framework _ _ ps) -> not . null $ ps) `filter` (a `removeIntersectingPlatforms` b)
    in  Just filteredEntries
  lhsMap = toRepositoryMap lhs
  rhsMap = toRepositoryMap rhs



-- | Builds a string representing the remote path to a framework zip archive.
remoteFrameworkPath :: Bool -> TargetPlatform -> InvertedRepositoryMap -> Framework -> Version -> String
remoteFrameworkPath x p r f v = remoteCacheDirectory p r f ++ frameworkArchiveName f v x



-- | Builds a `String` representing the remote path to a dSYM zip archive
remoteDsymPath :: TargetPlatform -> InvertedRepositoryMap -> Framework -> Version -> String
remoteDsymPath p r f v = remoteCacheDirectory p r f ++ dSYMArchiveName f v



-- | Builds a `String` representing the remote path to a bcsymbolmap zip archive
remoteBcsymbolmapPath :: DwarfUUID -> TargetPlatform -> InvertedRepositoryMap -> Framework -> Version -> String
remoteBcsymbolmapPath d p r f v = remoteCacheDirectory p r f ++ bcsymbolmapArchiveName d v



-- | Builds a `String` representing the name of the remote cache directory for a
-- | given conbination of `TargetPlatform` and `Framework` based on an
-- | `InvertedRepositoryMap`.
remoteCacheDirectory :: TargetPlatform -> InvertedRepositoryMap -> Framework -> String
remoteCacheDirectory p r f = repoName </> show p ++ "/" where repoName = unProjectName $ repoNameForFrameworkName r f



-- | Builds a `String` representing the name of the VersionFile for a given
-- | `ProjectNameAndVersion`
remoteVersionFilePath :: ProjectNameAndVersion -> String
remoteVersionFilePath (projectName, version) =
  unProjectName projectName </> versionFileNameForProjectNameVersioned projectName version



-- | Builds a `String` representing the path to the Carthage build directory for
-- | a combination of `TargetPlatform` and `Framework` representing
-- | the path to the framework's bundle
frameworkBuildBundleForPlatform :: TargetPlatform -> Framework -> String
frameworkBuildBundleForPlatform p f = carthageArtifactsBuildDirectoryForPlatform p f </> appendFrameworkExtensionTo f



-- | Constructs a `RepositoryMap` from a list of `RomefileEntry`s.
-- | The keys are `ProjectName`s.
toRepositoryMap :: [RomefileEntry] -> RepositoryMap
toRepositoryMap = M.fromList . map romeFileEntryToTuple



-- | Constructs an `InvertedRepositoryMap` from a list of `RomefileEntry`s.
-- | The keys are `FrameworkName`s.
toInvertedRepositoryMap :: [RomefileEntry] -> InvertedRepositoryMap
toInvertedRepositoryMap = M.fromList . concatMap romeFileEntryToListOfTuples
 where
  listify (fs, g) = map (\f -> (f, g)) fs
  flipTuple                   = uncurry (flip (,))
  romeFileEntryToListOfTuples = listify . flipTuple . romeFileEntryToTuple



-- | Creates a tuple out of a `RomefileEntry`.
romeFileEntryToTuple :: RomefileEntry -> (ProjectName, [Framework])
romeFileEntryToTuple RomefileEntry {..} = (_projectName, _frameworks)



-- | Performs a lookup in an `InvertedRepositoryMap` for a certain `Framework`.
-- | Creates a `ProjectName` from just the `frameworkName` of a `FrameworkName`
-- | in case the lookup fails.
repoNameForFrameworkName :: InvertedRepositoryMap -> Framework -> ProjectName
repoNameForFrameworkName reverseRomeMap framework =
  fromMaybe (ProjectName . _frameworkName $ framework) (M.lookup framework reverseRomeMap)



-- | Given an `InvertedRepositoryMap` and a list of  `FrameworkVersion` produces
-- | a list of __unique__ `ProjectNameAndVersion`s
repoNamesAndVersionForFrameworkVersions :: InvertedRepositoryMap -> [FrameworkVersion] -> [ProjectNameAndVersion]
repoNamesAndVersionForFrameworkVersions reverseRomeMap versions =
  nub $ zip (map (repoNameForFrameworkName reverseRomeMap . _framework) versions) (map _frameworkVersion versions)



-- | Given a `ProjectName` produces the appropriate file name for the corresponding
-- | Carthage VersionFile
versionFileNameForProjectName :: ProjectName -> String
versionFileNameForProjectName prjn = "." <> unProjectName prjn <> ".version"



-- | Given a `ProjectName` produces the appropriate file name for the corresponding
-- | Carthage VersionFile with appenended `Version` information
versionFileNameForProjectNameVersioned :: ProjectName -> Version -> String
versionFileNameForProjectNameVersioned prjn version = versionFileNameForProjectName prjn <> "-" <> unVersion version



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
  availabilityPrefix (PlatformAvailability _ True ) = "+"
  availabilityPrefix (PlatformAvailability _ False) = "-"
  platformName = show . _availabilityPlatform



-- | Given a `RepositoryMap` and a list of `CartfileEntry` creates a list of
-- | `FrameworkVersion`s. See `deriveFrameworkNameAndVersion` for details.
deriveFrameworkNamesAndVersion :: RepositoryMap -> [CartfileEntry] -> [FrameworkVersion]
deriveFrameworkNamesAndVersion romeMap = concatMap (deriveFrameworkNameAndVersion romeMap)



-- | Matches the current HEAD commit to a tag
-- | Returns the HEAD commit hash in case there is no match
deriveCurrentVersion :: MonadIO m => ExceptT String m Version
deriveCurrentVersion = do
  (revparseExitCode, headCommit, revparseErrorText) <- Turtle.procStrictWithErr "git"
                                                                                ["rev-parse", "HEAD"]
                                                                                (return $ Turtle.unsafeTextToLine "")
  case revparseExitCode of
    Turtle.ExitSuccess -> do
      (describeExitCode, version, _) <- Turtle.procStrictWithErr
        "git"
        ["describe", "--tags", "--exact-match", T.stripEnd headCommit]
        (return $ Turtle.unsafeTextToLine "")
      case describeExitCode of
        Turtle.ExitSuccess -> return $ Version (T.unpack $ T.stripEnd version)
        _                  -> return $ Version (T.unpack $ T.stripEnd headCommit)
    _ -> throwError $ if (not . T.null) revparseErrorText
      then T.unpack $ errorMessageHeader <> revparseErrorText
      else T.unpack $ errorMessageHeader <> unknownErrorText
 where
  unknownErrorText   = "Unknown git error"
  errorMessageHeader = "Git Error: "


-- | Given a `RepositoryMap` and a `CartfileEntry` creates a list of
-- | `FrameworkVersion` by attaching the `Version` information from the
-- | `FrameworkName` in the `CartfileEntry`.
deriveFrameworkNameAndVersion :: RepositoryMap -> CartfileEntry -> [FrameworkVersion]
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry _ _ v) = map (`FrameworkVersion` v) $ fromMaybe
  [Framework repositoryName Dynamic allTargetPlatforms]
  (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
  where repositoryName = unProjectName $ gitRepoNameFromCartfileEntry cfe


-- | Given a `RepositoryMap` and a list of `ProjectName`s produces another
-- | `RepositoryMap` containing only those `ProjectName`s.
filterRepoMapByGitRepoNames :: RepositoryMap -> [ProjectName] -> RepositoryMap
filterRepoMapByGitRepoNames repoMap gitRepoNames =
  M.unions $ map (restrictRepositoryMapToGitRepoName repoMap) gitRepoNames



-- | Given an `InvertedRepositoryMap` and a list of `FrameworkAvailability`s
-- | produces the corresponding list of `ProjectAvailability`s.
getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities
  :: InvertedRepositoryMap -> [FrameworkAvailability] -> [ProjectAvailability]
getMergedGitRepoAvailabilitiesFromFrameworkAvailabilities reverseRomeMap =
  concatMap mergeRepoAvailabilities . groupAvailabilities . getGitRepoAvalabilities
 where
  getGitRepoAvalabilities :: [FrameworkAvailability] -> [ProjectAvailability]
  getGitRepoAvalabilities = fmap getGitRepoAvailabilityFromFrameworkAvailability

  getGitRepoAvailabilityFromFrameworkAvailability :: FrameworkAvailability -> ProjectAvailability
  getGitRepoAvailabilityFromFrameworkAvailability (FrameworkAvailability (FrameworkVersion fwn v) availabilities) =
    ProjectAvailability (repoNameForFrameworkName reverseRomeMap fwn) v availabilities

  groupAvailabilities :: [ProjectAvailability] -> [[ProjectAvailability]]
  groupAvailabilities = groupBy ((==) `on` _availabilityProject) . sortBy (compare `on` _availabilityProject)

  -- | Given a list of `ProjectAvailability`s produces a singleton list of
  -- | `ProjectAvailability`s containing all `PlatformAvailability`s of the
  -- | original list.
  --
  -- >>> let g1 = ProjectAvailability (ProjectName "Alamofire") (Version "1") [PlatformAvailability IOS True]
  -- >>> let g2 = ProjectAvailability (ProjectName "Alamofire") (Version "2") [PlatformAvailability MacOS True]
  -- >>> let g3 = ProjectAvailability (ProjectName "CoreStore") (Version "3") [PlatformAvailability TVOS True]
  -- >>> mergeRepoAvailabilities [g1, g2, g3]
  -- [ProjectAvailability {_availabilityRepo = ProjectName {unProjectName = "Alamofire"}
  --                      , _availabilityVersion = Version {unVersion = "1"}
  --                      , _repoPlatformAvailabilities = [PlatformAvailability {_availabilityPlatform = iOS, _isAvailable = True}
  --                                                      ,PlatformAvailability {_availabilityPlatform = macOS, _isAvailable = True}
  --                                                      ,PlatformAvailability {_availabilityPlatform = tvOS, _isAvailable = True}]
  --                      }
  -- ]
  mergeRepoAvailabilities :: [ProjectAvailability] -> [ProjectAvailability]
  mergeRepoAvailabilities []                         = []
  mergeRepoAvailabilities repoAvailabilities@(x : _) = [x { _repoPlatformAvailabilities = platformAvailabilities }]
   where
    groupedPlatformAvailabilities :: [[PlatformAvailability]]
    groupedPlatformAvailabilities =
      sortAndGroupPlatformAvailabilities (repoAvailabilities >>= _repoPlatformAvailabilities)

    bothAvailable :: PlatformAvailability -> PlatformAvailability -> PlatformAvailability
    bothAvailable p p' = p { _isAvailable = _isAvailable p && _isAvailable p' }

    platformAvailabilities :: [PlatformAvailability]
    platformAvailabilities = fmap (foldl1 bothAvailable) groupedPlatformAvailabilities

    sortAndGroupPlatformAvailabilities :: [PlatformAvailability] -> [[PlatformAvailability]]
    sortAndGroupPlatformAvailabilities =
      groupBy ((==) `on` _availabilityPlatform) . sortBy (compare `on` _availabilityPlatform)



--- | Take a path and makes it absolute resolving ../ and ~
--- See https://www.schoolofhaskell.com/user/dshevchenko/cookbook/transform-relative-path-to-an-absolute-path
absolutizePath :: FilePath -> IO FilePath
absolutizePath aPath
  | "~" `T.isPrefixOf` T.pack aPath = do
    homePath <- getHomeDirectory
    return $ normalise $ addTrailingPathSeparator homePath ++ Prelude.tail aPath
  | otherwise = canonicalizePath aPath



-- | Creates a Zip archive of a file system path
createZipArchive
  :: MonadIO m
  => FilePath -- ^ The path to Zip.
  -> Bool -- ^ A flag controlling verbosity.
  -> ExceptT String m Zip.Archive
createZipArchive filePath verbose = do
  fileExists     <- liftIO $ doesFileExist filePath
  directoryExist <- liftIO $ doesDirectoryExist filePath
  if fileExists || directoryExist
    then do
      when verbose $ sayLnWithTime $ "Starting to zip: " <> filePath
      liftIO $ Zip.addFilesToArchive [Zip.OptRecursive, Zip.OptPreserveSymbolicLinks] Zip.emptyArchive [filePath]
    else throwError $ "Error: " <> filePath <> " does not exist"



-- | Adds executable permissions to a Framework. See https://github.com/blender/Rome/issues/57
makeExecutable
  :: MonadIO m
  => String -- ^ Path to the file
  -> m Turtle.Permissions
makeExecutable path = Turtle.chmod Turtle.executable $ Turtle.fromString path

-- | Perform an action on a file if it exists
ifExists :: MonadIO m => String -> m a -> m (Maybe a)
ifExists path fileAction = do
  fileExists <- liftIO $ doesFileExist path
  if fileExists then Just <$> fileAction else return Nothing

-- | Delete a directory an all it's contents
deleteDirectory
  :: MonadIO m
  => FilePath -- ^ The path to the directory to delete
  -> Bool -- ^ A flag controlling verbosity
  -> m ()
deleteDirectory path verbose = do
  directoryExists <- liftIO $ doesDirectoryExist path
  let sayFunc = if verbose then sayLnWithTime else sayLn
  when directoryExists $ do
    Turtle.rmtree . Turtle.fromString $ path
    when verbose $ sayFunc $ "Deleted: " <> path



-- | Delete a file
deleteFile
  :: MonadIO m
  => FilePath -- ^ The path to the directory to delete
  -> Bool -- ^ A flag controlling verbosity
  -> m ()
deleteFile path verbose = do
  let sayFunc = if verbose then sayLnWithTime else sayLn
  liftIO $ removeFile path
  when verbose $ liftIO . sayFunc $ "Deleted: " <> path



-- | Deletes a Framework from the Carthage Build folder
deleteFrameworkDirectory
  :: MonadIO m
  => FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework to delete
  -> TargetPlatform -- ^ The `TargetPlatform` to restrict this operation to
  -> Bool -- ^ A flag controlling verbosity
  -> m ()
deleteFrameworkDirectory (FrameworkVersion f _) platform = deleteDirectory frameworkDirectory
 where
  frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
  platformBuildDirectory              = carthageArtifactsBuildDirectoryForPlatform platform f
  frameworkDirectory                  = platformBuildDirectory </> frameworkNameWithFrameworkExtension



-- | Deletes a dSYM from the Carthage Build folder
deleteDSYMDirectory
  :: MonadIO m
  => FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM to delete
  -> TargetPlatform -- ^ The `TargetPlatform` to restrict this operation to
  -> Bool -- ^ A flag controlling verbosity
  -> m ()
deleteDSYMDirectory (FrameworkVersion f _) platform = deleteDirectory dSYMDirectory
 where
  frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
  platformBuildDirectory              = carthageArtifactsBuildDirectoryForPlatform platform f
  dSYMDirectory                       = platformBuildDirectory </> frameworkNameWithFrameworkExtension <> ".dSYM"



-- | Unzips a zipped (as in zip compression) `LBS.ByteString` in the current directory.
unzipBinary
  :: MonadIO m
  => LBS.ByteString -- ^ `LBS.The ByteString`.
  -> String -- ^ A colloquial name for the `LBS.ByteString` printed when verbose is `True`.
  -> String -- ^ A colloquial name for the artifact printed when verbose is `True`. Does not influence the artifact's name on disk.
  -> Bool -- ^ A verbostiry flag.
  -> m ()
unzipBinary objectBinary objectName objectZipName verbose = do
  when verbose $ sayLnWithTime $ "Starting to unzip " <> objectZipName
  if LBS.length objectBinary == 0
    then when verbose $ sayLnWithTime $ "Warning: " <> objectZipName <> " is ZERO bytes"
    else do
      liftIO $ Zip.extractFilesFromArchive [Zip.OptRecursive, Zip.OptPreserveSymbolicLinks] (Zip.toArchive objectBinary)
      when verbose $ sayLnWithTime $ "Unzipped " <> objectName <> " from: " <> objectZipName



-- | Saves a ByteString to file
saveBinaryToFile
  :: (MonadUnliftIO m, MonadIO m)
  => LBS.ByteString -- ^ The `ByteString` to save.
  -> FilePath -- ^ The destination path.
  -> m ()
saveBinaryToFile binaryArtifact destinationPath = do
  liftIO $ createDirectoryIfMissing True (dropFileName destinationPath)
  runResourceT $ C.runConduit $ C.sourceLbs binaryArtifact C..| C.sinkFile destinationPath



redControlSequence :: String
redControlSequence = "\ESC[0;31m"



greenControlSequence :: String
greenControlSequence = "\ESC[0;32m"



noColorControlSequence :: String
noColorControlSequence = "\ESC[0m"



third :: (a, b, c) -> c
third (_, _, c) = c



toJSONStr :: ToJSON a => a -> String
toJSONStr = T.unpack . decodeUtf8 . LBS.toStrict . encode



whenLeft :: Monad m => (l -> m ()) -> Either l r -> m ()
whenLeft f (Left  e) = f e
whenLeft _ (Right _) = return ()



-- | Read a file as `Text` and perform an action
fromFile
  :: MonadIO m
  => FilePath -- ^ The `FilePath` to the file to read
  -> (T.Text -> ExceptT String m a) -- ^ The action
  -> ExceptT String m a
fromFile f action = do
  file <- liftIO (T.readFile f)
  withExceptT (("Could not parse " <> f <> ": ") <>) (action file)

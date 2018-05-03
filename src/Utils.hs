{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Utils where

import qualified Codec.Archive.Zip            as Zip
import           Configuration                (carthageBuildDirectoryForPlatform)
import           Control.Arrow                (left)
import           Control.Exception            as E (try)
import           Control.Lens                 hiding (List)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import           Data.Char                    (isNumber)
import qualified Data.Conduit                 as C (($$))
import qualified Data.Conduit.Binary          as C (sinkFile, sourceLbs)
import           Data.Function                (on)
import           Data.List
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromJust, fromMaybe)
import           Data.Monoid
import           Data.Romefile
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.IO                 as T
import           Data.Time
import qualified Network.AWS                  as AWS (Error, ErrorMessage (..),
                                                      serviceMessage,
                                                      _ServiceError)
import           Network.HTTP.Conduit         as HTTP
import           Network.HTTP.Types.Header    as HTTP (hUserAgent)
import           Numeric                      (showFFloat)
import           System.Directory             (createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               doesFileExist, getHomeDirectory,
                                               removeFile)
import           System.FilePath              (addTrailingPathSeparator,
                                               dropFileName, normalise, (</>))
import           System.IO.Error              (isDoesNotExistError)
import           System.Path.NameManip        (absolute_path, guess_dotdot)
import           Text.Read                    (readMaybe)
import qualified Turtle
import           Types
import           Xcode.DWARF                  (DwarfUUID, bcsymbolmapNameFrom)


-- | Pretty print a `RomeVersion`
romeVersionToString :: RomeVersion -> String
romeVersionToString (major, minor, patch, build) = show major
                                                  <> "."
                                                  <> show minor
                                                  <> "."
                                                  <> show patch
                                                  <> "."
                                                  <> show build

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

-- | Given a `FrameworkName` and a `Version` produces a name
-- | for a bcsymbolmap Zip archive.
bcsymbolmapArchiveName :: DwarfUUID -> Version -> String
bcsymbolmapArchiveName d (Version v) =  bcsymbolmapNameFrom d ++ "-" ++ v ++ ".zip"



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
gitRepoNameFromCartfileEntry (CartfileEntry Binary (Location l) _) = GitRepoName . T.unpack . T.replace ".json" "" . last . splitWithSeparator '/' . T.pack $ l



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



-- | Builds a string representing the remote path to a framework zip archive.
remoteFrameworkPath :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> Version -> String
remoteFrameworkPath p r f v = remoteCacheDirectory p r f ++ frameworkArchiveName f v

-- | Builds a `String` representing the remote path to a dSYM zip archive
remoteDsymPath :: TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> Version -> String
remoteDsymPath p r f v = remoteCacheDirectory p r f ++ dSYMArchiveName f v

-- | Builds a `String` representing the remote path to a bcsymbolmap zip archive
remoteBcsymbolmapPath :: DwarfUUID -> TargetPlatform -> InvertedRepositoryMap -> FrameworkName -> Version -> String
remoteBcsymbolmapPath d p r f v = remoteCacheDirectory p r f ++ bcsymbolmapArchiveName d v



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
    availabilityPrefix (PlatformAvailability _ True)  = "+"
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
deriveFrameworkNameAndVersion romeMap cfe@(CartfileEntry _ _ v) = map (`FrameworkVersion` v) $
  fromMaybe [FrameworkName repositoryName] (M.lookup (gitRepoNameFromCartfileEntry cfe) romeMap)
  where
    repositoryName = unGitRepoName $ gitRepoNameFromCartfileEntry cfe



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



-- | Creates a Zip archive of a file system path
createZipArchive :: MonadIO m
       => FilePath -- ^ The path to Zip.
       -> Bool -- ^ A flag controlling verbosity.
       -> ExceptT String m Zip.Archive
createZipArchive filePath verbose = do
  fileExists <- liftIO $ doesFileExist filePath
  directoryExist <- liftIO $ doesDirectoryExist filePath
  if fileExists || directoryExist
    then do
      when verbose $
          sayLnWithTime $ "Staring to zip: " <> filePath
      liftIO $ Zip.addFilesToArchive [Zip.OptRecursive, Zip.OptPreserveSymbolicLinks] Zip.emptyArchive [filePath]
    else throwError $ "Error: " <> filePath <> " does not exist"



-- | Adds executable permissions to a Framework. See https://github.com/blender/Rome/issues/57
makeExecutable :: MonadIO m
               => TargetPlatform -- ^ The `TargetPlatform` to limit the operation to
               -> FrameworkName -- ^ The name of the Framework
               -> m Turtle.Permissions
makeExecutable p fname = Turtle.chmod Turtle.executable
                        (
                          Turtle.fromString $
                            frameworkBuildBundleForPlatform p fname
                            </> unFrameworkName fname
                        )



-- | Delete a directory an all it's contents
deleteDirectory :: MonadIO m
                => FilePath -- ^ The path to the directory to delete
                -> Bool -- ^ A flag controlling verbosity
                -> m ()
deleteDirectory path
                verbose = do
  directoryExists <- liftIO $ doesDirectoryExist path
  let sayFunc = if verbose then sayLnWithTime else sayLn
  when directoryExists $ do
    Turtle.rmtree . Turtle.fromString $ path
    when verbose $
      sayFunc $ "Deleted: " <> path



-- | Delete a file
deleteFile :: MonadIO m
           => FilePath -- ^ The path to the directory to delete
           -> Bool -- ^ A flag controlling verbosity
           -> m ()
deleteFile path
          verbose = do
  let sayFunc = if verbose then sayLnWithTime else sayLn
  liftIO $ removeFile path `catch` handleError sayFunc
  when verbose $
      liftIO . sayFunc $ "Deleted: " <> path
  where
    handleError f e
      | isDoesNotExistError e = f $ "Error: no such file " <> path
      | otherwise = throwM e



-- | Deletes a Framework from the Carthage Build folder
deleteFrameworkDirectory :: MonadIO m
                         => FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework to delete
                         -> TargetPlatform -- ^ The `TargetPlatform` to restrict this operation to
                         -> Bool -- ^ A flag controlling verbosity
                         -> m ()
deleteFrameworkDirectory (FrameworkVersion f _)
                         platform =
  deleteDirectory frameworkDirectory
  where
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    frameworkDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension



-- | Deletes a dSYM from the Carthage Build folder
deleteDSYMDirectory :: MonadIO m
                    => FrameworkVersion -- ^ The `FrameworkVersion` identifying the dSYM to delete
                    -> TargetPlatform -- ^ The `TargetPlatform` to restrict this operation to
                    -> Bool -- ^ A flag controlling verbosity
                    -> m ()
deleteDSYMDirectory (FrameworkVersion f _)
                    platform =
  deleteDirectory dSYMDirectory
  where
    frameworkNameWithFrameworkExtension = appendFrameworkExtensionTo f
    platformBuildDirectory = carthageBuildDirectoryForPlatform platform
    dSYMDirectory = platformBuildDirectory </> frameworkNameWithFrameworkExtension <> ".dSYM"



-- | Unzips a zipped (as in zip compression) `LBS.ByteString` in the current directory.
unzipBinary :: MonadIO m
            => LBS.ByteString -- ^ `LBS.The ByteString`.
            -> String -- ^ A colloquial name for the `LBS.ByteString` printed when verbose is `True`.
            -> String -- ^ A colloquial name for the artifact printed when verbose is `True`. Does not influence the artifact's name on disk.
            -> Bool -- ^ A verbostiry flag.
            -> m ()
unzipBinary objectBinary objectName objectZipName verbose = do
  when verbose $
   sayLnWithTime $ "Staring to unzip " <> objectZipName
  liftIO $ Zip.extractFilesFromArchive [Zip.OptRecursive, Zip.OptPreserveSymbolicLinks] (Zip.toArchive objectBinary)
  when verbose $
    sayLnWithTime $ "Unzipped " <> objectName <> " from: " <> objectZipName



-- | Saves a ByteString to file
saveBinaryToFile :: MonadIO m
                 => LBS.ByteString -- ^ The `ByteString` to save.
                 -> FilePath -- ^ The destination path.
                 -> m ()
saveBinaryToFile binaryArtifact destinationPath = do
  liftIO $ createDirectoryIfMissing True (dropFileName destinationPath)
  liftIO . runResourceT $ C.sourceLbs binaryArtifact C.$$ C.sinkFile destinationPath



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
whenLeft f (Left e)  = f e
whenLeft _ (Right _) = return ()



-- | Read a file as `Text` and pefrom an action
fromFile :: MonadIO m
         => FilePath -- ^ The `FilePath` to the file to read
         -> (T.Text -> ExceptT String m a) -- ^ The action
         -> ExceptT String m a
fromFile f action = do
  file <- liftIO (T.readFile f)
  withExceptT (("Could not parse " <> f <> ": ") <>) (action file)

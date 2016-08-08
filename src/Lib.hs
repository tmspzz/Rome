{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module Lib
    ( parseRomeOptions
    , getS3Configuration
    , donwloadORUpload
    ) where





import qualified Aws
import qualified Aws.S3                       as S3
import qualified Codec.Archive.Zip            as Zip
import           Control.Applicative          ((<|>))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.Trans          (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import           Data.Conduit                 (($$+-), ($=))
import           Data.Conduit.Binary          (sinkLbs)
import qualified Data.Map                     as M
import           Data.Maybe
import qualified Data.Text                    as T
import           Network.HTTP.Conduit         (Manager, RequestBody (..),
                                               newManager, responseBody,
                                               tlsManagerSettings)
import           Options.Applicative          as Opts
import           System.Directory
import qualified Text.Parsec                  as Parsec
import           Text.Parsec.String


type Location = String
type Version = String
type Config = (Aws.Configuration, S3.S3Configuration Aws.NormalQuery, Bool)
type RomeMonad = ExceptT String IO

data RepoHosting = GitHub | Git
  deriving (Eq, Show)

data CartfileEntry = CartfileEntry { hosting  :: RepoHosting
                                   , location :: Location
                                   , version  :: Version
                                   }
                                   deriving (Show, Eq)

data RomefileEntry = RomefileEntry { gitRepositoryName   :: String
                                   , frameworkCommonName :: String
                                   }
                                   deriving (Show, Eq)

data RomeCommand = Upload [String]
                  | Download [String]
                  deriving (Show, Eq)

data RomeOptions = RomeOptions { romeCommand :: RomeCommand
                               , verbose     :: Bool
                               }




uploadParser :: Opts.Parser RomeCommand
uploadParser = pure Upload <*> Opts.many (Opts.argument str (Opts.metavar "FRAMEWORKS..." <> Opts.help "Zero or more framework names as specified in the Cartfile. If zero, all frameworks are uploaded."))

downloadParser :: Opts.Parser RomeCommand
downloadParser = pure Download <*> Opts.many (Opts.argument str (Opts.metavar "FRAMEWORKS..." <> Opts.help "Zero or more framework names as specified in the Cartfile. If zero, all frameworks are downloaded."))

parseRomeCommand :: Opts.Parser RomeCommand
parseRomeCommand = Opts.subparser $
  Opts.command "upload" (uploadParser `withInfo` "Uploads frameworks contained in the local Carthage/Build/iOS to S3, according to the local Cartfile.resolved")
  <> Opts.command "download" (downloadParser `withInfo` "Downdloads and unpacks in Carthage/Build/iOS frameworks found in S3, according to the local Carftfile.resolved")

parseRomeOptions :: Opts.Parser RomeOptions
parseRomeOptions = RomeOptions <$> parseRomeCommand <*> Opts.switch ( Opts.short 'v' <> help "Show verbose output" )

withInfo :: Opts.Parser a -> String -> Opts.ParserInfo a
withInfo opts desc = Opts.info (Opts.helper <*> opts) $ Opts.progDesc desc

cartfileResolved :: String
cartfileResolved = "Cartfile.resolved"

romefile :: String
romefile = "Romefile"

getCartfileEntires :: RomeMonad [CartfileEntry]
getCartfileEntires = do
  eitherCartfileEntries <- liftIO $ parseFromFile (Parsec.many1 parseCartfileResolvedLine) cartfileResolved
  case eitherCartfileEntries of
    Left e -> throwError $ "Carfile.resolved parse error: " ++ show e
    Right cartfileEntries -> return cartfileEntries

getRomefileEntries :: RomeMonad (S3.Bucket, [RomefileEntry])
getRomefileEntries = do
  romeConfig <- liftIO $ parseFromFile parseRomeConfig romefile
  case romeConfig of
    Left e -> throwError $ "Romefile parse error: " ++ show e
    Right (bucketName, entries) -> return (T.pack bucketName, entries)

donwloadORUpload :: Aws.Configuration -> S3.S3Configuration Aws.NormalQuery -> RomeOptions -> ExceptT String IO ()
donwloadORUpload cfg s3cfg (RomeOptions options verbose) = do
  cartfileEntries <- getCartfileEntires
  (s3BucketName, romefileEntries) <- getRomefileEntries
  case options of
      Upload [] -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom cartfileEntries romefileEntries
        liftIO $ runReaderT (uploadFrameworksToS3 s3BucketName frameworkAndVersions) (cfg, s3cfg, verbose)

      Upload names ->
        liftIO $ runReaderT (uploadFrameworksToS3 s3BucketName (filterByNames cartfileEntries romefileEntries names)) (cfg, s3cfg, verbose)

      Download [] -> do
        let frameworkAndVersions = constructFrameworksAndVersionsFrom cartfileEntries romefileEntries
        liftIO $ runReaderT (downloadFrameworksFromS3 s3BucketName frameworkAndVersions) (cfg, s3cfg, verbose)

      Download names ->
        liftIO $ runReaderT (downloadFrameworksFromS3 s3BucketName (filterByNames cartfileEntries romefileEntries names)) (cfg, s3cfg, verbose)
  where
   constructFrameworksAndVersionsFrom cartfileEntries romefileEntries = zip (deriveFrameworkNames (toRomeFilesEntriesMap romefileEntries) cartfileEntries) (map version cartfileEntries)
   filterByNames cartfileEntries romefileEntries = concatMap (constructFrameworksAndVersionsFrom cartfileEntries romefileEntries `filterByName`)


getS3Configuration :: MonadIO m => m (Aws.Configuration, S3.S3Configuration Aws.NormalQuery)
getS3Configuration = do
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
  return (cfg, s3cfg)

filterByName:: [(String, Version)] -> String -> [(String, Version)]
filterByName fs s = filter (\(name, version) -> name == s) fs

uploadFrameworksToS3 :: (MonadIO m) => S3.Bucket -> [(String, Version)] -> ReaderT Config m ()
uploadFrameworksToS3 s3Bucket s = do
  manager <- liftIO $ newManager tlsManagerSettings
  mapM_ (uploadFrameworkToS3 manager s3Bucket) s

uploadFrameworkToS3 :: (MonadIO m) => Manager -> S3.Bucket -> (String, Version) -> ReaderT Config m ()
uploadFrameworkToS3 manager s3Bucket (framework, version) = do
  let pathInCarthageBuild =  appendFrameworkExtensionTo $ "Carthage/Build/iOS/" ++ framework
  exists <- liftIO $ doesDirectoryExist pathInCarthageBuild
  when exists $ do
    (_, _, verbose) <- ask
    archive <- liftIO $ Zip.addFilesToArchive (zipOptions verbose) Zip.emptyArchive [pathInCarthageBuild]
    uploadB manager s3Bucket (Zip.fromArchive archive) (framework ++ "/" ++ appendFrameworkExtensionTo framework ++ "-" ++ version ++ ".zip")

downloadFrameworksFromS3 :: (MonadIO m) => S3.Bucket -> [(String, Version)] -> ReaderT Config m ()
downloadFrameworksFromS3  s3Bucket s = do
  manager <- liftIO $ newManager tlsManagerSettings
  mapM_ (downloadFrameworkFromS3 manager s3Bucket) s

downloadFrameworkFromS3 :: (MonadIO m) => Manager -> S3.Bucket -> (String, Version) -> ReaderT Config m ()
downloadFrameworkFromS3 manager s3Bucket (frameworkName, version) = do
  (awsConfig, s3Config, verbose) <- ask
  liftIO $
      {- Create a request object with S3.getObject and run the request with pureAws. -}
      runResourceT $ do
        let frameworkZipName = appendFrameworkExtensionTo frameworkName ++ "-" ++ version ++ ".zip"
        let frameworkRemotePath = frameworkName ++ "/" ++ frameworkZipName
        e <- Aws.aws awsConfig s3Config manager $ S3.getObject s3Bucket (T.pack frameworkRemotePath)
        let eitherResponse = Aws.responseResult e
        case eitherResponse of
          Left exception -> liftIO . putStrLn $ "Could not download:  " ++ frameworkZipName
          Right S3.GetObjectResponse { .. } -> do
            lbs <- responseBody gorResponse $$+- sinkLbs
            liftIO . putStrLn $ "Donwloaded: " ++ frameworkZipName
            lift $ Zip.extractFilesFromArchive (zipOptions verbose) (Zip.toArchive lbs)
            liftIO . putStrLn $ "Unzipped: " ++ frameworkZipName

zipOptions :: Bool -> [Zip.ZipOption]
zipOptions verbose = if verbose then [Zip.OptRecursive, Zip.OptVerbose] else [Zip.OptRecursive]

uploadB :: (MonadIO m) => Manager -> S3.Bucket -> L.ByteString -> String -> ReaderT Config m ()
uploadB manager s3Bucket lazyByteString destinationPath = do
  contents <- ask
  (awsConfig, s3Config, verbose) <- ask
  liftIO $
    runResourceT $ do
    let body = RequestBodyLBS lazyByteString
    response <- Aws.pureAws awsConfig s3Config manager $
      S3.putObject s3Bucket (T.pack destinationPath) body
    liftIO . putStrLn $ "Uploaded: " ++ destinationPath

deriveFrameworkNames :: M.Map String String -> [CartfileEntry] -> [String]
deriveFrameworkNames romeMap = map (deriveFrameworkName romeMap)

deriveFrameworkName ::  M.Map String String -> CartfileEntry -> String
deriveFrameworkName romeMap (CartfileEntry GitHub l _) = last $ splitWithSeparator '/' l
deriveFrameworkName romeMap (CartfileEntry Git l _)    = fromMaybe "" (M.lookup (getGitRepositoryNameFromGitURL l) romeMap >>= \x -> Just x)
  where
    getGitRepositoryNameFromGitURL = reverse . tail . snd . splitAt 3 . reverse . last . splitWithSeparator '/'

appendFrameworkExtensionTo :: String -> String
appendFrameworkExtensionTo a = a ++ ".framework"

splitWithSeparator :: (Eq a) => a -> [a] -> [[a]]
splitWithSeparator _ [] = []
splitWithSeparator a as = g as : splitWithSeparator a (dropTaken as as)
    where
      numberOfAsIn = length . takeWhile (== a)
      g = takeWhile (/= a) . dropWhile (== a)
      dropTaken bs = drop $ numberOfAsIn bs + length (g bs)



-- Cartfile.resolved parsing

parseGitHub :: Parsec.Parsec String () RepoHosting
parseGitHub = Parsec.string "github" >> Parsec.many1 Parsec.space >> pure GitHub

parseGit :: Parsec.Parsec String () RepoHosting
parseGit = Parsec.string "git" >> Parsec.many1 Parsec.space >> pure Git

repoHosting :: Parsec.Parsec String () RepoHosting
repoHosting = Parsec.try parseGit <|> parseGitHub

quotedContent :: Parsec.Parsec String () String
quotedContent = do
  Parsec.char '"'
  location <- parseUnquotedString
  Parsec.char '"'
  return location

parseCartfileResolvedLine :: Parsec.Parsec String () CartfileEntry
parseCartfileResolvedLine = do
  hosting <- repoHosting
  location <- quotedContent
  Parsec.many1 Parsec.space
  version <- quotedContent
  Parsec.endOfLine
  return CartfileEntry {..}



-- Romefile parsing

parseS3BucketNameSection :: Parsec.Parsec String () String
parseS3BucketNameSection = do
  Parsec.string "[S3Bucket]" >> Parsec.endOfLine
  s3BucketName <- parseWhiteSpaces >> parseUnquotedString
  Parsec.endOfLine
  return s3BucketName

parseRepositoryMapSection :: Parsec.Parsec String () [RomefileEntry]
parseRepositoryMapSection = do
  Parsec.string "[RepositoryMap]" >> Parsec.endOfLine
  Parsec.many parseRepositoryMapLine

parseRepositoryMapLine :: Parsec.Parsec String () RomefileEntry
parseRepositoryMapLine = do
  gitRepositoryName <- parseWhiteSpaces >> parseUnquotedString
  frameworkCommonName <- parseWhiteSpaces >> parseUnquotedString
  Parsec.endOfLine
  return RomefileEntry {..}

parseWhiteSpaces :: Parsec.Parsec String () String
parseWhiteSpaces =  Parsec.try (Parsec.many1 Parsec.space) <|> Parsec.many1 Parsec.tab

parseUnquotedString :: Parsec.Parsec String () String
parseUnquotedString = Parsec.many1 (Parsec.noneOf ['"', ' ', '\t', '\n', '\'', '\\', '\r'])

parseRomeConfig :: Parsec.Parsec String () (String, [RomefileEntry])
parseRomeConfig = do
  s3BucketName <- parseS3BucketNameSection
  Parsec.many Parsec.newline
  romeFileEntries <- Parsec.option [] parseRepositoryMapSection
  Parsec.manyTill parseWhiteSpaces Parsec.eof
  return (s3BucketName, romeFileEntries)

toRomeFilesEntriesMap :: [RomefileEntry] -> M.Map String String
toRomeFilesEntriesMap = M.fromList . map romeFileEntryToTuple

romeFileEntryToTuple :: RomefileEntry -> (String, String)
romeFileEntryToTuple RomefileEntry {..} = (gitRepositoryName, frameworkCommonName)

parse rule text = Parsec.parse rule "(source)" text

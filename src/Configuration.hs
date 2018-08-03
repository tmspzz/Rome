module Configuration where


import           Control.Applicative             ((<|>))
import           Control.Arrow                   (left)
import           Control.Monad.Except
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import           Data.Yaml                       (decodeFileEither, prettyPrintParseException)
import           Data.Monoid                     ((<>))
import           Data.Romefile
import qualified Data.Text.IO                    as T
import           System.Directory
import           System.FilePath
import           Types


getCartfileEntires :: RomeMonad [CartfileEntry]
getCartfileEntires = do
  eitherCartfileEntries <- parseCartfileResolved cartfileResolved
  case eitherCartfileEntries of
    Left e -> throwError $ "Carfile.resolved parse error: " ++ show e
    Right cartfileEntries -> return cartfileEntries

getRomefileEntries :: RomeMonad RomeFile
getRomefileEntries = 
  let fromYaml = ExceptT $ left prettyPrintParseException <$> decodeFileEither romefileName
      fromIni = ExceptT $ parseRomefile <$> T.readFile romefileName 
      in
        withExceptT toErr $ fromYaml <|> fromIni
  where toErr e = "Error while parsing " <> romefileName <> ": " <> e

getS3ConfigFile :: MonadIO m => m FilePath
getS3ConfigFile = (</> awsConfigFilePath) `liftM` liftIO getHomeDirectory
  where awsConfigFilePath = ".aws/config"

-- carthageBuildDirectoryForPlatform :: TargetPlatform -> FilePath
-- carthageBuildDirectoryForPlatform platform = carthageBuildDirectory </> show platform

carthageBuildDirectory :: FilePath
carthageBuildDirectory = "Carthage" </> "Build"

carthageArtifactsBuildDirectoryForPlatform
  :: TargetPlatform -> Framework -> FilePath
carthageArtifactsBuildDirectoryForPlatform platform (Framework n Dynamic) =
  carthageBuildDirectory </> show platform
carthageArtifactsBuildDirectoryForPlatform platform (Framework n Static) =
  carthageBuildDirectory </> show platform </> "Static"

module Configuration where


import           Control.Monad.Except
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
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

getRomefileEntries :: RomeMonad RomeFileParseResult
getRomefileEntries = withExceptT toErr $ ExceptT $ parseRomefile <$> T.readFile romefile
  where
    toErr e = "Error while parsing " <> romefile <> ": " <> e

getS3ConfigFile :: MonadIO m => m FilePath
getS3ConfigFile = (</> awsConfigFilePath) `liftM` liftIO getHomeDirectory
  where
      awsConfigFilePath = ".aws/config"

carthageBuildDirectoryForPlatform :: TargetPlatform -> FilePath
carthageBuildDirectoryForPlatform platform = carthageBuildDirectory </> show platform

carthageBuildDirectory :: FilePath
carthageBuildDirectory = "Carthage" </> "Build"

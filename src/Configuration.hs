module Configuration where


import           Control.Monad.Except
import           Data.Cartfile
import           Data.Romefile
import           System.Directory
import           System.FilePath
import           Types
import           Types.TargetPlatform


getCartfileEntires :: RomeMonad [CartfileEntry]
getCartfileEntires = do
  eitherCartfileEntries <- liftIO $ parseCartfileResolved cartfileResolved
  case eitherCartfileEntries of
    Left e -> throwError $ "Carfile.resolved parse error: " ++ show e
    Right cartfileEntries -> return cartfileEntries

getRomefileEntries :: RomeMonad RomeFileParseResult
getRomefileEntries = parseRomefile romefile

getS3ConfigFile :: MonadIO m => m FilePath
getS3ConfigFile = (</> awsConfigFilePath) `liftM` liftIO getHomeDirectory
  where
      awsConfigFilePath = ".aws/config"

carthageBuildDirectory :: TargetPlatform -> FilePath
carthageBuildDirectory platform = "Carthage" </> "Build" </> show platform

module Main where

import           Control.Monad.Except
import           Lib
import           Network.AWS          as AWS
import           Options.Applicative  as Opts



romeVersion :: String
romeVersion = "0.3.0.1"



-- Main
main :: IO ()
main = do
  let opts = info (Opts.helper <*> Opts.flag' Nothing (Opts.long "version" <> Opts.help "Prints the version information" <> Opts.hidden ) <|> Just <$> parseRomeOptions) (header "S3 cache tool for Carthage" )
  cmd <- execParser opts
  case cmd of
    Nothing -> putStrLn $ romeVersion ++ " - Romam uno die non fuisse conditam."
    Just romeOptions -> do
      env <- AWS.newEnv AWS.NorthVirginia AWS.Discover
      l <- runExceptT $ runRomeWithOptions env romeOptions
      case l of
        Right _ -> return ()
        Left e -> putStrLn e

module Main where

import           CommandParsers     (parseRomeOptions)
import           Control.Monad.Except
import           Lib
import           Network.AWS          as AWS
import           Options.Applicative  as Opts



romeVersion :: String
romeVersion = "0.10.2.23"



-- Main
main :: IO ()
main = do
  let opts = info (Opts.helper <*> Opts.flag' Nothing (Opts.long "version" <> Opts.help "Prints the version information" <> Opts.hidden ) <|> Just <$> parseRomeOptions) (header "S3 cache tool for Carthage" )
  cmd <- execParser opts
  case cmd of
    Nothing -> putStrLn $ romeVersion ++ " - Romam uno die non fuisse conditam."
    Just romeOptions -> do
      p <- runExceptT $ do
        r   <- discoverRegion
        env <- AWS.newEnv r AWS.Discover
        runRomeWithOptions env romeOptions
      case p of
        Right _ -> return ()
        Left e -> putStrLn e

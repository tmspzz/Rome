module Main where

import           CommandParsers       (parseRomeOptions)
import           Control.Monad.Except
import           Lib
import           Options.Applicative  as Opts
import           System.Exit



romeVersion :: String
romeVersion = "0.11.0.26"



-- Main
main :: IO ()
main = do
  let opts = info (Opts.helper <*> Opts.flag' Nothing (Opts.long "version" <> Opts.help "Prints the version information" <> Opts.hidden ) <|> Just <$> parseRomeOptions) (header "S3 cache tool for Carthage" )
  cmd <- execParser opts
  case cmd of
    Nothing -> putStrLn $ romeVersion ++ " - Romam uno die non fuisse conditam."
    Just romeOptions -> do
      p <- runExceptT $ runRomeWithOptions romeOptions
      case p of
        Right _ -> return ()
        Left e  -> die e

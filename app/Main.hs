module Main where

import           Control.Monad.Except
import           Options.Applicative          as Opts
import           Lib



romeVersion :: String
romeVersion = "0.1.0.0"



-- Main
main :: IO ()
main = do
  let opts = info (Opts.helper <*> Opts.flag' Nothing (Opts.long "version" <> Opts.help "Prints the version information" <> Opts.hidden ) <|> Just <$> parseRomeOptions) (header "S3 cache tool for Carthage" )
  cmd <- execParser opts
  (cfg, s3cfg) <- getS3Configuration
  case cmd of
    Nothing -> putStrLn $ romeVersion ++ " - Romam uno die non fuisse conditam."
    Just romeOptions -> do
      l <- runExceptT $ donwloadORUpload cfg s3cfg romeOptions
      case l of
        Right _ -> return ()
        Left e -> putStrLn e

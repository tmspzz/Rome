module Main where

import           Data.Monoid          ((<>))
import           CommandParsers       (parseRomeOptions)
import           Control.Monad.Except
import           Lib
import           Options.Applicative  as Opts
import           System.Exit



romeVersion :: RomeVersion
romeVersion = (0, 12, 0, 31)



-- Main
main :: IO ()
main = do
  let opts = info (Opts.helper <*> Opts.flag' Nothing (Opts.long "version" <> Opts.help "Prints the version information" <> Opts.hidden ) <|> Just <$> parseRomeOptions) (header "S3 cache tool for Carthage" )
  cmd <- execParser opts
  case cmd of
    Nothing -> putStrLn $ romeVersionToString romeVersion ++ " - Romam uno die non fuisse conditam."
    Just romeOptions -> do
      p <- runExceptT $ runRomeWithOptions romeOptions romeVersion
      case p of
        Right _ -> return ()
        Left e  -> die e

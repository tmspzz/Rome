module Main where

import           CommandParsers                           ( parseRomeOptions )
import           Control.Monad.Except
import           Control.Monad.Reader                     ( runReaderT )
import           Data.Monoid                              ( (<>) )
import           Data.UUID.V4                  as UUID
                                                          ( nextRandom )
import           Lib
import           Options.Applicative           as Opts
import           System.Exit



romeVersion :: RomeVersion
romeVersion = (0, 23, 2, 62)



-- Main
main :: IO ()
main = do
  let opts = info
        (   Opts.helper
        <*> Opts.flag' Nothing (Opts.long "version" <> Opts.help "Prints the version information" <> Opts.hidden)
        <|> Just
        <$> parseRomeOptions
        )
        (header "Cache tool for Carthage")
  cmd <- execParser opts
  case cmd of
    Nothing          -> putStrLn $ romeVersionToString romeVersion ++ " - Romam uno die non fuisse conditam."
    Just romeOptions -> do
      globalUUID <- UUID.nextRandom
      p          <- runReaderT (runExceptT (runRomeWithOptions romeOptions romeVersion)) globalUUID
      case p of
        Right _ -> return ()
        Left  e -> die e

{-# LANGUAGE FlexibleContexts #-}

module CommandParsers where

import           Data.Char            (isLetter)
import           Data.Either.Utils    (maybeToEither)
import           Data.List            (nub)
import           Data.List.Split      (wordsBy)
import           Data.Romefile
import           Options.Applicative  as Opts
import           Text.Read            (readMaybe)
import           Types.Commands
import           Types.TargetPlatform



{- Commnad line arguments parsing -}

-- verifyParser :: Parser VerifyFlag
-- verifyParser = VerifyFlag <$> Opts.switch ( Opts.long "verify" <> Opts.help "Verify that the framework has the same hash as specified in the Cartfile.resolved.")


skipLocalCacheParser :: Parser SkipLocalCacheFlag
skipLocalCacheParser = SkipLocalCacheFlag <$> Opts.switch ( Opts.long "skip-local-cache" <> Opts.help "Ignore the local cache when performing the operation.")

reposParser :: Opts.Parser [GitRepoName]
reposParser = Opts.many (Opts.argument (GitRepoName <$> str) (Opts.metavar "FRAMEWORKS..." <> Opts.help "Zero or more framework names. If zero, all frameworks and dSYMs are uploaded."))

platformsParser :: Opts.Parser [TargetPlatform]
platformsParser = (nub . concat <$> Opts.some (Opts.option (eitherReader platformListOrError) (Opts.metavar "PLATFORMS" <> Opts.long "platform" <> Opts.help "Applicable platforms for the command. One of iOS, MacOS, tvOS, watchOS, or a comma-separated list of any of these values.")))
  <|> pure allTargetPlatforms
  where
    platformOrError str = maybeToEither ("Unrecognized platform '" ++ str ++ "'") (readMaybe str)
    splitPlatforms str = filter (not . null) $ filter isLetter <$> wordsBy (not . isLetter) str
    platformListOrError str = mapM platformOrError $ splitPlatforms str

udcPayloadParser :: Opts.Parser RomeUDCPayload
udcPayloadParser = RomeUDCPayload <$> reposParser <*> platformsParser {- <*> verifyParser-} <*> skipLocalCacheParser

uploadParser :: Opts.Parser RomeCommand
uploadParser = pure Upload <*> udcPayloadParser

downloadParser :: Opts.Parser RomeCommand
downloadParser = pure Download <*> udcPayloadParser

listModeParser :: Opts.Parser ListMode
listModeParser = (
                    Opts.flag' Missing (Opts.long "missing" <> Opts.help "List frameworks missing from the cache. Ignores dSYMs")
                    <|> Opts.flag' Present (Opts.long "present" <> Opts.help "List frameworks present in the cache. Ignores dSYMs.")
                 )
                <|> Opts.flag All All (Opts.help "Reports missing or present status of frameworks in the cache. Ignores dSYMs.")

listPayloadParser :: Opts.Parser RomeListPayload
listPayloadParser = RomeListPayload <$> listModeParser <*> platformsParser

listParser :: Opts.Parser RomeCommand
listParser = List <$> listPayloadParser

parseRomeCommand :: Opts.Parser RomeCommand
parseRomeCommand = Opts.subparser $
  Opts.command "upload" (uploadParser `withInfo` "Uploads frameworks and dSYMs contained in the local Carthage/Build/<platform> to S3, according to the local Cartfile.resolved")
  <> Opts.command "download" (downloadParser `withInfo` "Downloads and unpacks in Carthage/Build/<platform> frameworks and dSYMs found in S3, according to the local Carftfile.resolved")
  <> Opts.command "list" (listParser `withInfo` "Lists frameworks in the cache and reports cache misses/hits, according to the local Carftfile.resolved. Ignores dSYMs.")

parseRomeOptions :: Opts.Parser RomeOptions
parseRomeOptions = RomeOptions <$> parseRomeCommand <*> Opts.switch ( Opts.short 'v' <> help "Show verbose output" )

withInfo :: Opts.Parser a -> String -> Opts.ParserInfo a
withInfo opts desc = Opts.info (Opts.helper <*> opts) $ Opts.progDesc desc

{-# LANGUAGE RecordWildCards #-}

module Data.Carthage.Cartfile
  ( parseCartfileResolved
  , cartfileResolved
  , CartfileEntry(..)
  , RepoHosting(..)
  , Version(..)
  , Location(..)
  )
where

import           Control.Applicative                      ( (<|>) )
import           Control.Monad.Trans                      ( MonadIO
                                                          , liftIO
                                                          )
import           Data.Maybe
import qualified Text.Parsec                   as Parsec
import qualified Text.Parsec.String            as Parsec
import qualified Text.Parsec.Utils             as Parsec
import           Data.Carthage.Common

newtype Location = Location { unLocation :: String }
                   deriving (Eq, Show, Ord)

data RepoHosting = GitHub | Git | Binary
  deriving (Eq, Show)

data CartfileEntry = CartfileEntry { hosting  :: RepoHosting
                                   , location :: Location
                                   , version  :: Version
                                   }
                                   deriving (Show, Eq)



cartfileResolved :: String
cartfileResolved = "Cartfile.resolved"

-- Cartfile.resolved parsing

parseGitHub :: Parsec.Parsec String () RepoHosting
parseGitHub = Parsec.string "github" >> Parsec.many1 Parsec.space >> pure GitHub

parseGit :: Parsec.Parsec String () RepoHosting
parseGit = Parsec.string "git" >> Parsec.many1 Parsec.space >> pure Git

parseBinary :: Parsec.Parsec String () RepoHosting
parseBinary = Parsec.string "binary" >> Parsec.many1 Parsec.space >> pure Binary

repoHosting :: Parsec.Parsec String () RepoHosting
repoHosting = Parsec.try parseGit <|> parseGitHub <|> parseBinary

quotedContent :: Parsec.Parsec String () String
quotedContent = Parsec.char '"' *> Parsec.parseUnquotedString <* Parsec.char '"'

parseCartfileEntry :: Parsec.Parsec String () CartfileEntry
parseCartfileEntry = do
  hosting  <- repoHosting
  location <- Location <$> quotedContent
  _        <- Parsec.many1 Parsec.space
  version  <- Version <$> quotedContent
  return CartfileEntry { .. }

parseCartfileResolved :: MonadIO m => String -> m (Either Parsec.ParseError [CartfileEntry])
parseCartfileResolved = liftIO . Parsec.parseFromFile
  (   catMaybes
  <$> (  (Parsec.many $ do
           line <-
             Parsec.optional Parsec.endOfLine
             *> (Parsec.try parseEmptyLine <|> Parsec.try parseDependency <|> Parsec.try parseComment)
             <* Parsec.optional Parsec.endOfLine
           case line of
             Dependency entry -> return $ Just entry
             _                -> return Nothing
         )
      <* Parsec.eof
      )
  )

data CartfileLine = EmptyLine | Comment | Dependency { entry :: CartfileEntry } deriving (Eq, Show)

parseDependency :: Parsec.Parsec String () CartfileLine
parseDependency = Dependency <$> parseCartfileEntry

parseComment :: Parsec.Parsec String () CartfileLine
parseComment = Parsec.char '#' >> Parsec.manyTill Parsec.anyChar (Parsec.lookAhead Parsec.endOfLine) >> return Comment

parseEmptyLine :: Parsec.Parsec String () CartfileLine
parseEmptyLine = Parsec.many1 Parsec.space >> return EmptyLine

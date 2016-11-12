{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Cartfile
    ( parseCartfileResolved
    , cartfileResolved
    , CartfileEntry (..)
    , RepoHosting (..)
    , Version (..)
    , Location (..)
    ) where


import           Control.Applicative ((<|>))

import qualified Text.Parsec         as Parsec
import qualified Text.Parsec.String  as Parsec
import qualified Text.Parsec.Utils   as Parsec

newtype Location = Location { unLocation :: String }
                   deriving (Eq, Show, Ord)
newtype Version  = Version { unVersion :: String }
                   deriving (Eq, Show, Ord)

data RepoHosting = GitHub | Git
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

repoHosting :: Parsec.Parsec String () RepoHosting
repoHosting = Parsec.try parseGit <|> parseGitHub

quotedContent :: Parsec.Parsec String () String
quotedContent = do
  Parsec.char '"'
  location <- Parsec.parseUnquotedString
  Parsec.char '"'
  return location

parseCartfileResolvedLine :: Parsec.Parsec String () CartfileEntry
parseCartfileResolvedLine = do
  hosting <- repoHosting
  location <- fmap Location quotedContent
  Parsec.many1 Parsec.space
  version <- fmap Version quotedContent
  Parsec.endOfLine
  return CartfileEntry {..}

parseCartfileResolved :: String -> IO (Either Parsec.ParseError [CartfileEntry])
parseCartfileResolved = Parsec.parseFromFile (Parsec.many1 (Parsec.optional Parsec.spaces >> parseCartfileResolvedLine))

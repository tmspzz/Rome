{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}


module Data.Romefile
    ( parseRomefile
    , romefile
    , RomefileEntry (..)
    , FrameworkName
    , GitRepoName
    )
where

import           Data.Cartfile
import qualified Text.Parsec         as Parsec
import qualified Text.Parsec.Utils   as Parsec
import qualified Text.Parsec.String  as Parsec

type FrameworkName = String
type GitRepoName   = String
data RomefileEntry = RomefileEntry { gitRepositoryName   :: GitRepoName
                                   , frameworkCommonName :: FrameworkName
                                   }
                                   deriving (Show, Eq)



romefile :: String
romefile = "Romefile"

-- Romefile parsing

parseS3BucketNameSection :: Parsec.Parsec String () String
parseS3BucketNameSection = do
  Parsec.string "[S3Bucket]" >> Parsec.endOfLine
  s3BucketName <- Parsec.parseWhiteSpaces >> Parsec.parseUnquotedString
  Parsec.endOfLine
  return s3BucketName

parseRepositoryMapSection :: Parsec.Parsec String () [RomefileEntry]
parseRepositoryMapSection = do
  Parsec.string "[RepositoryMap]" >> Parsec.endOfLine
  Parsec.many parseRepositoryMapLine

parseRepositoryMapLine :: Parsec.Parsec String () RomefileEntry
parseRepositoryMapLine = do
  gitRepositoryName <- Parsec.parseWhiteSpaces >> Parsec.parseUnquotedString
  frameworkCommonName <- Parsec.parseWhiteSpaces >> Parsec.parseUnquotedString
  Parsec.endOfLine
  return RomefileEntry {..}

parseRomeConfig :: Parsec.Parsec String () (String, [RomefileEntry])
parseRomeConfig = do
  s3BucketName <- parseS3BucketNameSection
  Parsec.many Parsec.newline
  romeFileEntries <- Parsec.option [] parseRepositoryMapSection
  Parsec.manyTill Parsec.parseWhiteSpaces Parsec.eof
  return (s3BucketName, romeFileEntries)

parseRomefile :: String -> IO (Either Parsec.ParseError (String, [RomefileEntry]))
parseRomefile = Parsec.parseFromFile parseRomeConfig

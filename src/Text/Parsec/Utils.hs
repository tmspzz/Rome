module Text.Parsec.Utils
    ( parseWhiteSpaces
    , parseUnquotedString
    ) where



import           Control.Applicative ((<|>))
import qualified Text.Parsec         as Parsec



parseWhiteSpaces :: Parsec.Parsec String () String
parseWhiteSpaces =  Parsec.try (Parsec.many1 Parsec.space) <|> Parsec.many1 Parsec.tab

parseUnquotedString :: Parsec.Parsec String () String
parseUnquotedString = Parsec.many1 (Parsec.noneOf ['"', ' ', '\t', '\n', '\'', '\\', '\r'])

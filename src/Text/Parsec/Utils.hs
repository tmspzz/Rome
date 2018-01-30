{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Utils
    ( parseWhiteSpaces
    , parseUnquotedString
    , onceAndConsumeTill
    ) where



import           Control.Applicative    ((<|>))
import           Data.Functor.Identity
import qualified Text.Parsec            as Parsec



parseWhiteSpaces :: Parsec.Parsec String () String
parseWhiteSpaces =  Parsec.try (Parsec.many1 Parsec.space) <|> Parsec.many1 Parsec.tab

parseUnquotedString :: Parsec.Parsec String () String
parseUnquotedString = Parsec.many1 (Parsec.noneOf ['"', ' ', '\t', '\n', '\'', '\\', '\r'])


-- | @onceOrConsumeTill p@ end@ tries to apply the parser @p@ /once/ and consumes
-- | the input until @end@. Returns a `Maybe` of the value of @p@.
-- | Thanks to Tobias Mayer, Berlin Haskell Group.
onceAndConsumeTill :: (Parsec.Stream s Identity Char)
                   => Parsec.Parsec s u a
                   -> Parsec.Parsec s u b
                   -> Parsec.Parsec s u (Maybe a)
onceAndConsumeTill p end = Parsec.optionMaybe (Parsec.try p) <* consume
                           where
                             consume = Parsec.try end <|> Parsec.anyChar *> consume

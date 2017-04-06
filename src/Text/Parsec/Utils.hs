{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Utils
    ( parseWhiteSpaces
    , parseUnquotedString
    , onceOrConsumeTill
    ) where



import           Control.Applicative ((<|>))
import qualified Text.Parsec         as Parsec
import Data.Functor.Identity



parseWhiteSpaces :: Parsec.Parsec String () String
parseWhiteSpaces =  Parsec.try (Parsec.many1 Parsec.space) <|> Parsec.many1 Parsec.tab

parseUnquotedString :: Parsec.Parsec String () String
parseUnquotedString = Parsec.many1 (Parsec.noneOf ['"', ' ', '\t', '\n', '\'', '\\', '\r'])


-- | @onceOrConsumeTill p@ end@ tries to apply the parser @p@ /once/ and consumes
-- | the input until @end@. Returns a `Maybe` of the value of @p@.
-- | Thanks to Tobias Mayer, Berlin Haskell Group.
onceOrConsumeTill :: (Parsec.Stream s Identity t, Show t)
                  => Parsec.Parsec s u a
                  -> Parsec.Parsec s u b
                  -> Parsec.Parsec s u (Maybe a)
onceOrConsumeTill p end = Parsec.optionMaybe p <* consume
                           where
                             consume = do { end; return ()}
                               <|> Parsec.anyToken *> consume

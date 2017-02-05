module Data.TargetPlatform
    ( allTargetPlatforms
    , TargetPlatform (..)
    ) where



import           Data.Char
import           Data.Maybe
import           Text.Read
import qualified Text.Read.Lex as L



data TargetPlatform = IOS | MacOS | TVOS | WatchOS
              deriving (Ord, Eq)

instance Show TargetPlatform where
  show IOS      = "iOS"
  show MacOS    = "macOS"
  show TVOS     = "tvOS"
  show WatchOS  = "watchOS"

allTargetPlatforms :: [TargetPlatform]
allTargetPlatforms = [IOS, MacOS, TVOS, WatchOS]

instance Read TargetPlatform where
  readPrec = parens $ do
    L.Ident s <- lexP
    case map toLower s of
       "ios"     -> return IOS
       "macos"   -> return MacOS
       "tvos"    -> return TVOS
       "watchos" -> return WatchOS
       a         -> error $ "Unrecognized platform " ++ a

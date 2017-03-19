module Types.Commands where

import           Data.Romefile
import           Data.Carthage.TargetPlatform

data RomeCommand = Upload RomeUDCPayload
                  | Download RomeUDCPayload
                  | List RomeListPayload
                  deriving (Show, Eq)

data RomeUDCPayload = RomeUDCPayload { _payload            :: [GitRepoName]
                                     , _udcPlatforms       :: [TargetPlatform]
                                    --  , _verifyFlag         :: VerifyFlag
                                     , _skipLocalCacheFlag :: SkipLocalCacheFlag
                                     }
                                     deriving (Show, Eq)

-- newtype VerifyFlag = VerifyFlag { _verify :: Bool } deriving (Show, Eq)

newtype SkipLocalCacheFlag = SkipLocalCacheFlag { _skipLocalCache :: Bool }
                                                deriving (Show, Eq)

data RomeListPayload = RomeListPayload { _listMode      :: ListMode
                                       , _listPlatforms :: [TargetPlatform]
                                       }
                                       deriving (Show, Eq)

data ListMode = All
               | Missing
               | Present
               deriving (Show, Eq)

data RomeOptions = RomeOptions { romeCommand :: RomeCommand
                               , verbose     :: Bool
                               }
                               deriving (Show, Eq)

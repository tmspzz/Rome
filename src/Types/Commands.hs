module Types.Commands where

import           Data.Romefile
import           Data.Carthage.TargetPlatform

data RomeCommand = Upload RomeUDCPayload
                  | Download RomeUDCPayload
                  | List RomeListPayload
                  | Utils RomeUtilsPayload
                  deriving (Show, Eq)



data RomeUDCPayload = RomeUDCPayload { _payload              :: [ProjectName]
                                     , _udcPlatforms         :: [TargetPlatform]
                                     , _cachePrefix          :: String
                                    --  , _verifyFlag         :: VerifyFlag
                                     , _skipLocalCacheFlag   :: SkipLocalCacheFlag
                                     , _noIgnoreFlag         :: NoIgnoreFlag
                                     , _noSkipCurrentFlag    :: NoSkipCurrentFlag
                                     , _useXcFrameworksFlag  :: UseXcFrameworksFlag
                                     , _concurrentlyFlag     :: ConcurrentlyFlag
                                     }
                                     deriving (Show, Eq)



data RomeUtilsPayload = RomeUtilsPayload { _subcommand :: RomeUtilsSubcommand }
                                         deriving (Show, Eq)

data RomeUtilsSubcommand = MigrateRomefile deriving (Show, Eq)

-- newtype VerifyFlag = VerifyFlag { _verify :: Bool } deriving (Show, Eq)

newtype SkipLocalCacheFlag = SkipLocalCacheFlag { _skipLocalCache :: Bool }
                                                deriving (Show, Eq)

newtype NoIgnoreFlag = NoIgnoreFlag { _noIgnore :: Bool }
                                    deriving (Show, Eq)

newtype NoSkipCurrentFlag = NoSkipCurrentFlag { _noSkipCurrent :: Bool }
                                              deriving (Show, Eq)

newtype UseXcFrameworksFlag = UseXcFrameworksFlag { _useXcFrameworks :: Bool }
                                              deriving (Show, Eq)

newtype ConcurrentlyFlag = ConcurrentlyFlag { _concurrently :: Bool }
                                              deriving (Show, Eq)

data RomeListPayload = RomeListPayload { _listMode                  :: ListMode
                                       , _listPlatforms             :: [TargetPlatform]
                                       , _listCachePrefix           :: String
                                       , _listFormat                :: PrintFormat
                                       , _listNoIgnoreFlag          :: NoIgnoreFlag
                                       , _listNoSkipCurrentFlag     :: NoSkipCurrentFlag
                                       , _listUseXcFrameworksFlag   :: UseXcFrameworksFlag
                                       }
                                       deriving (Show, Eq)

data PrintFormat = Text
                 | JSON
                 deriving (Show, Eq, Read)

data ListMode = All
               | Missing
               | Present
               deriving (Show, Eq)

data RomeOptions = RomeOptions { romeCommand :: RomeCommand
                               , romefilePath :: FilePath
                               , verbose     :: Bool
                               }
                               deriving (Show, Eq)

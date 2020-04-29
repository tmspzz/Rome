module Main where

import           Control.Arrow                            ( left
                                                          , right
                                                          )
import           Control.Monad
import           Data.Carthage.Cartfile
import           Data.Carthage.TargetPlatform
import           Data.Either                              ( rights )
import           Data.List                                ( intercalate
                                                          , nub
                                                          , intersect
                                                          , isInfixOf
                                                          )
import           Data.Yaml                                ( decodeEither'
                                                          , encode
                                                          )
import           Data.Romefile
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Text.Parsec                   as Parsec
import           Types
import           Utils
import           Xcode.DWARF

import           Test.Hspec
import           Test.QuickCheck

nonEmptyString :: Gen String
nonEmptyString = listOf1 arbitrary

instance Arbitrary FrameworkVersion where
  arbitrary = liftM2 FrameworkVersion arbitrary arbitrary

instance Arbitrary FrameworkType where
  arbitrary = oneof $ fmap return [Dynamic, Static]

instance Arbitrary Framework where
  arbitrary = Framework <$> nonEmptyString <*> arbitrary <*> (nub <$> listOf1 arbitrary)

instance Arbitrary TargetPlatform where
  arbitrary = oneof $ fmap return [IOS, MacOS, WatchOS, TVOS]

instance Arbitrary Version where
  arbitrary = Version <$> nonEmptyString

prop_filterByNameEqualTo_idempotent :: [FrameworkVersion] -> Framework -> Bool
prop_filterByNameEqualTo_idempotent ls n =
  filterByFrameworkEqualTo ls n == filterByFrameworkEqualTo (filterByFrameworkEqualTo ls n) n

prop_filterByNameEqualTo_smaller :: [FrameworkVersion] -> Framework -> Bool
prop_filterByNameEqualTo_smaller ls n = length (filterByFrameworkEqualTo ls n) <= length ls

prop_filterByNameEqualTo_model :: [FrameworkVersion] -> Framework -> Bool
prop_filterByNameEqualTo_model ls n =
  map _framework (filterByFrameworkEqualTo ls n) == filter (== n) (map _framework ls)

prop_filterOutFrameworkNamesAndVersionsIfNotIn_idempotent :: [FrameworkVersion] -> [Framework] -> Bool
prop_filterOutFrameworkNamesAndVersionsIfNotIn_idempotent ls ns =
  filterOutFrameworksAndVersionsIfNotIn ls ns
    == filterOutFrameworksAndVersionsIfNotIn (filterOutFrameworksAndVersionsIfNotIn ls ns) ns

prop_filterOutFrameworkNamesAndVersionsIfNotIn_smaller :: [FrameworkVersion] -> [Framework] -> Bool
prop_filterOutFrameworkNamesAndVersionsIfNotIn_smaller ls ns =
  length (filterOutFrameworksAndVersionsIfNotIn ls ns) <= length ls

prop_filterOutFrameworkNamesAndVersionsIfNotIn_filterAllOut :: [Version] -> [Framework] -> Bool
prop_filterOutFrameworkNamesAndVersionsIfNotIn_filterAllOut vs fws =
  null $ (FrameworkVersion <$> fws <*> vs) `filterOutFrameworksAndVersionsIfNotIn` fws

prop_split_length :: Char -> String -> Property
prop_split_length sep ls =
  not (null ls) ==> length (splitWithSeparator sep (T.pack ls)) == 1 + length (filter (== sep) ls)

prop_split_string :: String -> Property
prop_split_string ls = not (null ls) ==> splitWithSeparator '/' (T.pack ls) == T.split (== '/') (T.pack ls)

data TestDwarfUUID = TDUUID String String Arch deriving Show

instance Arbitrary TestDwarfUUID where
  arbitrary = do
    uuid <- arbitraryUUID
    arch <- arbitraryArch
    return $ TDUUID (toInputLine uuid arch) uuid arch
   where
    toInputLine uuid arch = "UUID: " ++ uuid ++ " (" ++ show arch ++ ") Carthage/Build/iOS/Foo.framework/Foo"
    arbitraryUUID = fmap
      (intercalate "-")
      (sequence [vectorOf 8 hexDigits, vectorOf 4 hexDigits, vectorOf 4 hexDigits, vectorOf 12 hexDigits])
    hexDigits     = elements (['A' .. 'F'] ++ ['0' .. '9'])
    arbitraryArch = arbitrary

instance Arbitrary Arch where
  arbitrary = oneof $ fmap return [ARMV7, ARM64, I386, X86_64, Other "foobar"]

instance Arbitrary ProjectName where
  arbitrary = ProjectName <$> nonEmptyString

instance Arbitrary RomefileEntry where
  arbitrary = RomefileEntry <$> arbitrary <*> listOf1 arbitrary

data TestRomefile = TestRomefile { hasLocalCache :: Bool
                                 , hasS3Bucket :: Bool
                                 , rMapEntries :: [RomefileEntry]
                                 , iMapEntries :: [RomefileEntry]
                                 } deriving Show

instance Arbitrary TestRomefile where
  arbitrary = do
    (blCache, bS3Bucket) <- arbitrary `suchThat` (\(a, b) -> a || b) :: Gen (Bool, Bool)
    TestRomefile blCache bS3Bucket <$> arbitrary <*> arbitrary

toIniText :: TestRomefile -> T.Text
toIniText r = T.pack $ "[Cache]\n" ++ if hasLocalCache r
  then "  local = ~/some/path\n"
  else "" ++ if hasS3Bucket r
    then "  S3-Bucket = some-bucket\n"
    else "" ++ if not . null $ rMapEntries r
      then "[RepositoryMap]\n" ++ intercalate "\n" (map toIniTextRE (rMapEntries r))
      else "" ++ if not . null $ iMapEntries r
        then "[IgnoreMap]\n" ++ intercalate "\n" (map toIniTextRE (iMapEntries r))
        else ""

toIniTextRE :: RomefileEntry -> String
toIniTextRE r = "  " ++ (unProjectName (_projectName r)) ++ " = " ++ f
  where f = intercalate " ," $ (map show) (_frameworks r)

prop_parse_dwarf_dumpUUID :: TestDwarfUUID -> Bool
prop_parse_dwarf_dumpUUID (TDUUID inputLine uuid arch) =
  Right (DwarfUUID uuid arch) == Parsec.parse parseDwarfdumpUUID "test" inputLine

prop_romefileINIToYamlToRomefile_idempotent_romefileINI :: TestRomefile -> Bool
prop_romefileINIToYamlToRomefile_idempotent_romefileINI t = rights [parseRomefile (toIniText t)]
  == rights [parseRomefile (toIniText t) >>= Right . encode >>= left show . decodeEither']

prop_filterRomeFileEntriesByPlatforms_idempotent :: [RomefileEntry] -> [RomefileEntry] -> Bool
prop_filterRomeFileEntriesByPlatforms_idempotent base filteringValues =
  base
    `filterRomeFileEntriesByPlatforms` filteringValues
    == (base `filterRomeFileEntriesByPlatforms` filteringValues)
    `filterRomeFileEntriesByPlatforms` filteringValues

prop_filterRomeFileEntriesByPlatforms_filters :: [RomefileEntry] -> [RomefileEntry] -> Bool
prop_filterRomeFileEntriesByPlatforms_filters base filteringValues =
  null $ (base `filterRomeFileEntriesByPlatforms` filteringValues) `intersect` filteringValues


prop_filterRomeFileEntriesByPlatforms_min :: [RomefileEntry] -> [RomefileEntry] -> Bool
prop_filterRomeFileEntriesByPlatforms_min base filteringValues =
  (length $ base `filterRomeFileEntriesByPlatforms` filteringValues) <= length base

binaryURLPathWithParameters :: Gen String
binaryURLPathWithParameters = do
   firstPart <- arbitrary
   secondPart <- arbitrary
   lastPart <- arbitrary `suchThat` (\a -> not $ "/" `isInfixOf` a || ".json" `isInfixOf` a)
   return $ "https://" ++ firstPart ++ "/" ++  secondPart ++ "/" ++ "binary.json?some_paramter=test&" ++ lastPart

prop_gitRepoNameFromCartfileEntry_alaways_binary :: String -> Bool
prop_gitRepoNameFromCartfileEntry_alaways_binary url = 
  gitRepoNameFromCartfileEntry (CartfileEntry Binary (Location url) (Version "")) == ProjectName "binary"

main :: IO ()
main = do

  putStrLn "prop_filterByNameEqualTo_idempotent"
  quickCheck (withMaxSuccess 1000 prop_filterByNameEqualTo_idempotent)

  putStrLn "prop_filterByNameEqualTo_smaller"
  quickCheck (withMaxSuccess 1000 prop_filterByNameEqualTo_smaller)

  putStrLn "prop_filterByNameEqualTo_model"
  quickCheck (withMaxSuccess 1000 prop_filterByNameEqualTo_model)

  putStrLn "prop_filterOutFrameworkNamesAndVersionsIfNotIn_idempotent"
  quickCheck (withMaxSuccess 1000 prop_filterOutFrameworkNamesAndVersionsIfNotIn_idempotent)

  putStrLn "prop_filterOutFrameworkNamesAndVersionsIfNotIn_smaller"
  quickCheck (withMaxSuccess 1000 prop_filterOutFrameworkNamesAndVersionsIfNotIn_smaller)

  putStrLn "prop_filterOutFrameworkNamesAndVersionsIfNotIn_filterAllOut"
  quickCheck (withMaxSuccess 1000 prop_filterOutFrameworkNamesAndVersionsIfNotIn_filterAllOut)

  putStrLn "prop_split_length"
  quickCheck (withMaxSuccess 1000 prop_split_length)

  putStrLn "prop_split_string"
  quickCheck (withMaxSuccess 1000 prop_split_string)

  putStrLn "prop_parse_dwarf_dumpUUID"
  quickCheck (withMaxSuccess 1000 prop_parse_dwarf_dumpUUID)

  putStrLn "prop_romefileINIToYamlToRomefile_idempotent_romefileINI"
  quickCheck (withMaxSuccess 1000 prop_romefileINIToYamlToRomefile_idempotent_romefileINI)

  putStrLn "prop_filterRomeFileEntriesByPlatforms_idempotent"
  quickCheck (withMaxSuccess 1000 prop_filterRomeFileEntriesByPlatforms_idempotent)

  putStrLn "prop_filterRomeFileEntriesByPlatforms_min"
  quickCheck (withMaxSuccess 1000 prop_filterRomeFileEntriesByPlatforms_min)

  putStrLn "prop_filterRomeFileEntriesByPlatforms_filters"
  quickCheck (withMaxSuccess 1000 prop_filterRomeFileEntriesByPlatforms_filters)

  putStrLn "prop_gitRepoNameFromCartfileEntry_alaways_binary"
  quickCheck (withMaxSuccess 1000 $ forAll binaryURLPathWithParameters prop_gitRepoNameFromCartfileEntry_alaways_binary)

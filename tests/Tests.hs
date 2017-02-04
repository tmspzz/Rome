module Main where

import           Control.Monad

import           Lib
import           Data.Cartfile
import           Data.GitRepoAvailability
import           Data.Romefile
import qualified Data.Text as T

import           Test.Hspec
import           Test.QuickCheck

nonEmptyString :: Gen String
nonEmptyString = listOf1 arbitrary

instance Arbitrary FrameworkVersion where
  arbitrary = liftM2 FrameworkVersion arbitrary arbitrary

instance Arbitrary FrameworkName where
  arbitrary = FrameworkName <$> nonEmptyString

instance Arbitrary Version where
  arbitrary = Version <$> nonEmptyString

prop_filterByNameEqualTo_idempotent :: [FrameworkVersion] -> FrameworkName -> Bool
prop_filterByNameEqualTo_idempotent ls n = filterByNameEqualTo ls n == filterByNameEqualTo (filterByNameEqualTo ls n) n

prop_filterByNameEqualTo_smaller :: [FrameworkVersion] -> FrameworkName -> Bool
prop_filterByNameEqualTo_smaller ls n = length (filterByNameEqualTo ls n) <= length ls

prop_filterByNameEqualTo_model :: [FrameworkVersion] -> FrameworkName -> Bool
prop_filterByNameEqualTo_model ls n = map _frameworkName (filterByNameEqualTo ls n) == filter (== n) (map _frameworkName ls)

prop_filterOutFrameworkNamesAndVersionsIfNotIn_idempotent :: [FrameworkVersion] -> [FrameworkName] -> Bool
prop_filterOutFrameworkNamesAndVersionsIfNotIn_idempotent ls ns = filterOutFrameworkNamesAndVersionsIfNotIn ls ns == filterOutFrameworkNamesAndVersionsIfNotIn (filterOutFrameworkNamesAndVersionsIfNotIn ls ns) ns

prop_filterOutFrameworkNamesAndVersionsIfNotIn_smaller :: [FrameworkVersion] -> [FrameworkName] -> Bool
prop_filterOutFrameworkNamesAndVersionsIfNotIn_smaller ls ns = length (filterOutFrameworkNamesAndVersionsIfNotIn ls ns) <= length ls

prop_filterOutFrameworkNamesAndVersionsIfNotIn_model :: [FrameworkVersion] -> [FrameworkName] -> Bool
prop_filterOutFrameworkNamesAndVersionsIfNotIn_model ls ns = map _frameworkName (filterOutFrameworkNamesAndVersionsIfNotIn ls ns) == filter (`notElem` ns) (map _frameworkName ls)

prop_split_length :: Char -> String -> Property
prop_split_length sep ls =
  not (null ls) ==>
    length (splitWithSeparator sep ls) == 1 + length (filter (== sep) ls)

prop_split_string :: String -> Property
prop_split_string ls =
  not (null ls) ==>
    map T.pack (splitWithSeparator '/' ls) == T.split (=='/') (T.pack ls)

main :: IO ()
main =
  do
    putStrLn "prop_filterByNameEqualTo_idempotent"
    quickCheck prop_filterByNameEqualTo_idempotent

    putStrLn "prop_filterByNameEqualTo_smaller"
    quickCheck prop_filterByNameEqualTo_smaller

    putStrLn "prop_filterByNameEqualTo_model"
    quickCheck prop_filterByNameEqualTo_model

    putStrLn "prop_filterOutFrameworkNamesAndVersionsIfNotIn_idempotent"
    quickCheck prop_filterOutFrameworkNamesAndVersionsIfNotIn_idempotent

    putStrLn "prop_filterOutFrameworkNamesAndVersionsIfNotIn_smaller"
    quickCheck prop_filterOutFrameworkNamesAndVersionsIfNotIn_smaller

    putStrLn "prop_filterOutFrameworkNamesAndVersionsIfNotIn_model"
    quickCheck prop_filterOutFrameworkNamesAndVersionsIfNotIn_model

    putStrLn "prop_split_length"
    quickCheck prop_split_length

    putStrLn "prop_split_string"
    quickCheck prop_split_string

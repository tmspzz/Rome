module Main where

import           Lib
import           Data.Cartfile
import           Data.Romefile
import qualified Data.Text as T

import           Test.Hspec
import           Test.QuickCheck

nonEmptyString :: Gen String
nonEmptyString = listOf1 arbitrary

instance Arbitrary FrameworkName where
  arbitrary = FrameworkName <$> nonEmptyString

instance Arbitrary Version where
  arbitrary = Version <$> nonEmptyString

prop_filter_idempotent :: [(FrameworkName, Version)] -> FrameworkName -> Bool
prop_filter_idempotent ls n = filterByNameEqualTo ls n == filterByNameEqualTo (filterByNameEqualTo ls n) n

prop_filter_smaller :: [(FrameworkName, Version)] -> FrameworkName -> Bool
prop_filter_smaller ls n = length (filterByNameEqualTo ls n) <= length ls

prop_filter_model :: [(FrameworkName, Version)] -> FrameworkName -> Bool
prop_filter_model ls n = map fst (filterByNameEqualTo ls n) == filter (== n) (map fst ls)

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
    putStrLn "prop_filter_idempotent"
    quickCheck prop_filter_idempotent

    putStrLn "prop_filter_smaller"
    quickCheck prop_filter_smaller

    putStrLn "prop_filter_model"
    quickCheck prop_filter_model

    putStrLn "prop_split_length"
    quickCheck prop_split_length

    putStrLn "prop_split_string"
    quickCheck prop_split_string

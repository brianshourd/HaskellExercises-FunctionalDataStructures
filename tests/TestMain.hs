module Main
where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit

import Chapter2

main :: IO ()
main = defaultMain tests
  where
    tests =
        [testGroup "Chapter 2"
            [testCase "Exercise 2.1: Suffixes" case_suffixes
            ,testProperty "Suffixes length is one more than number of elements" prop_suffixesLength
            ,testProperty "Empty Set contains no elements" prop_emptySetEmpty
            ,testProperty "Single element set contains element" prop_setContainsInsert
            ,testProperty "Multi insert doesn't change" prop_setMultiInsert
            ,testProperty "Multi insert contains all elements" prop_setMultiInsertContains
            ,testProperty "Multi insert doesn't contain extras" prop_setMultiInsertOnlyContains
            ]
        ]

case_suffixes :: Assertion
case_suffixes = suffixes [1,2,3,4] @?= ([[1,2,3,4],[2,3,4],[3,4],[4],[]] :: [[Int]])

prop_suffixesLength :: [Char] -> Bool
prop_suffixesLength s = length (suffixes s) == (length s) + 1

prop_emptySetEmpty :: Int -> Bool
prop_emptySetEmpty x = member empty x == False

prop_setContainsInsert :: Int -> Bool
prop_setContainsInsert x = member (insert empty x) x == True

prop_setMultiInsert :: Int -> Bool
prop_setMultiInsert x = insert (insert empty x) x == insert empty x where

prop_setMultiInsertContains :: [Int] -> Bool
prop_setMultiInsertContains xs = all (member s) xs where
    s = setFromList xs

prop_setMultiInsertOnlyContains :: [Int] -> Int -> Bool
prop_setMultiInsertOnlyContains xs y = member s y == elem y xs where
    s = setFromList xs

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
            ,testProperty "Exercise 2.1: Suffixes length" prop_suffixesLength
            ]
        ]

case_suffixes :: Assertion
case_suffixes = suffixes [1,2,3,4] @?= ([[1,2,3,4],[2,3,4],[3,4],[4],[]] :: [[Int]])

prop_suffixesLength :: [Char] -> Bool
prop_suffixesLength s = length (suffixes s) == (length s) + 1

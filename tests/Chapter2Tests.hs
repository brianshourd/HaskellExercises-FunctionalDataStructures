module Chapter2Tests where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit

import Chapter2

chapter2TestGroup = testGroup "Chapter 2"
    [testGroup "Exercise 2.1: Suffixes"
        [testCase "Suffixes of [1,2,3,4]" case_suffixes
        ,testProperty "Suffixes length is one more than number of elements" prop_suffixesLength
        ]
    ,testGroup "Binary Tree Set tests"
        [testProperty "Empty Set contains no elements" prop_emptySetEmpty
        ,testProperty "Single element set contains element" prop_setContainsInsert
        ,testProperty "Multi insert doesn't change" prop_setMultiInsert
        ,testProperty "Multi insert contains all elements" prop_setMultiInsertContains
        ,testProperty "Multi insert doesn't contain extras" prop_setMultiInsertOnlyContains
        ,testCase "depth of [1..20]" case_depthWorst
        ,testProperty "depth is smaller than number of elements" prop_depthWorst
        ]
    ,testGroup "Exercise 2.2: Fewer comparisons for member function"
        [testProperty "Multi insert contains all elements, member'" prop_setMultiInsertContainsMember'
        ,testProperty "Multi insert doesn't contain extras, member'" prop_setMultiInsertOnlyContainsMember'
        ,testProperty "Multi insert contains all elements, betterMember'" prop_setMultiInsertContainsBetterMember'
        ,testProperty "Multi insert doesn't contain extras, betterMember'" prop_setMultiInsertOnlyContainsBetterMember'
        ,testProperty "betterMember' uses fewer than 2d + 1 comparisons (main problem)" prop_betterMember'IsBetter
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

prop_setMultiInsertContainsMember' :: [Int] -> Bool
prop_setMultiInsertContainsMember' xs = all (fst . member' s) xs where
    s = setFromList xs

prop_setMultiInsertOnlyContainsMember' :: [Int] -> Int -> Bool
prop_setMultiInsertOnlyContainsMember' xs y = (fst $ member' s y) == elem y xs where
    s = setFromList xs

prop_setMultiInsertContainsBetterMember' :: [Int] -> Bool
prop_setMultiInsertContainsBetterMember' xs = all (fst . betterMember' s) xs where
    s = setFromList xs

prop_setMultiInsertOnlyContainsBetterMember' :: [Int] -> Int -> Bool
prop_setMultiInsertOnlyContainsBetterMember' xs y = (fst $ betterMember' s y) == elem y xs where
    s = setFromList xs

case_depthWorst :: Assertion
case_depthWorst = depth (setFromList [1..20] :: Tree Int) @?= 20

prop_depthWorst :: [Int] -> Bool
prop_depthWorst xs = depth (setFromList xs) <= length xs

prop_betterMember'IsBetter :: [Int] -> Int -> Bool
prop_betterMember'IsBetter xs x = (snd $ betterMember' s x) <= 2 * (depth s) + 1 where
    s = setFromList xs

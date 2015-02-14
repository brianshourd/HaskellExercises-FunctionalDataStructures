module Chapter2Tests (chapter2TestGroup) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck (Gen, choose, Property, forAll)
import Data.List (nub)

import Chapter2

chapter2TestGroup :: Test.Framework.Test
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
        ,testCase "depth of [2,1,3,4]" case_depth2
        ,testProperty "depth is smaller than number of elements" prop_depthWorst
        ]
    ,testGroup "Exercise 2.2: Fewer comparisons for member function"
        [testProperty "Multi insert contains all elements, member'" prop_setMultiInsertContainsMember'
        ,testProperty "Multi insert doesn't contain extras, member'" prop_setMultiInsertOnlyContainsMember'
        ,testProperty "Multi insert contains all elements, betterMember'" prop_setMultiInsertContainsBetterMember'
        ,testProperty "Multi insert doesn't contain extras, betterMember'" prop_setMultiInsertOnlyContainsBetterMember'
        ,testProperty "betterMember' uses fewer than 2d + 1 comparisons (main problem)" prop_betterMember'IsBetter
        ]
    ,testGroup "Exercise 2.3: A better insert function"
        [testProperty "Multi betterInsert contains all elements" prop_setMultiBetterInsertContains
        ,testProperty "Multi betterInsert doesn't contain extras" prop_setMultiBetterInsertOnlyContains
        ]
    ,testGroup "Exercise 2.5: Building balanced trees"
        [testProperty "Size of a tree from a list is the number of uniqe elements in the list" prop_sizeTreeFromList
        ,testProperty "complete has correct depth" prop_completeCorrectDepth
        ,testProperty "complete has correct size" prop_completeCorrectSize
        ,testProperty "balancedTree has correct size" prop_balancedTreeCorrectSize
        ,testProperty "balancedTree is balanced" prop_balancedTreeBalanced
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
    s = treeFromList xs

prop_setMultiInsertOnlyContains :: [Int] -> Int -> Bool
prop_setMultiInsertOnlyContains xs y = member s y == elem y xs where
    s = treeFromList xs

prop_setMultiInsertContainsMember' :: [Int] -> Bool
prop_setMultiInsertContainsMember' xs = all (fst . member' s) xs where
    s = treeFromList xs

prop_setMultiInsertOnlyContainsMember' :: [Int] -> Int -> Bool
prop_setMultiInsertOnlyContainsMember' xs y = (fst $ member' s y) == elem y xs where
    s = treeFromList xs

prop_setMultiInsertContainsBetterMember' :: [Int] -> Bool
prop_setMultiInsertContainsBetterMember' xs = all (fst . betterMember' s) xs where
    s = treeFromList xs

prop_setMultiInsertOnlyContainsBetterMember' :: [Int] -> Int -> Bool
prop_setMultiInsertOnlyContainsBetterMember' xs y = (fst $ betterMember' s y) == elem y xs where
    s = treeFromList xs

case_depthWorst :: Assertion
case_depthWorst = depth (treeFromList [1..20] :: Tree Int) @?= 20

case_depth2 :: Assertion
case_depth2 = depth (treeFromList [2,1,3,4] :: Tree Int) @?= 3

prop_depthWorst :: [Int] -> Bool
prop_depthWorst xs = depth (treeFromList xs) <= length xs

prop_betterMember'IsBetter :: [Int] -> Int -> Bool
prop_betterMember'IsBetter xs x = (snd $ betterMember' s x) <= 2 * (depth s) + 1 where
    s = treeFromList xs

prop_setMultiBetterInsertContains :: [Int] -> Bool
prop_setMultiBetterInsertContains xs = all (member s) xs where
    s = foldl betterInsert empty xs

prop_setMultiBetterInsertOnlyContains :: [Int] -> Int -> Bool
prop_setMultiBetterInsertOnlyContains xs y = member s y == elem y xs where
    s = foldl betterInsert empty xs

prop_sizeTreeFromList :: [Int] -> Bool
prop_sizeTreeFromList xs = size (treeFromList xs) == length (nub xs)

-- I don't want large depths, it takes too long. Also doesn't make sense for
-- negative numbers
completeInts :: Gen Int
completeInts = choose (1, 20)

prop_completeCorrectDepth :: Char -> Property
prop_completeCorrectDepth x = forAll completeInts $ \d -> depth (complete x d) == d

prop_completeCorrectSize :: Char -> Property
prop_completeCorrectSize x = forAll completeInts $ \d -> size (complete x d) == 2^d - 1

balancedTreeInts :: Gen Int
balancedTreeInts = choose (1, 100000)

prop_balancedTreeCorrectSize :: Char -> Property
prop_balancedTreeCorrectSize x = forAll balancedTreeInts $ \s -> size (balancedTree x s) == s

prop_balancedTreeBalanced :: Char -> Property
prop_balancedTreeBalanced x = forAll balancedTreeInts $ impl where
    impl s = abs ((size l) - (size r)) <= 1 where
        (T l _ r) = balancedTree x s


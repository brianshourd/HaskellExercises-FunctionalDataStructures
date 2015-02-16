module Chapter3Tests (chapter3TestGroup) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck.Modifiers
import Data.List (sort)

import Chapter3

chapter3TestGroup :: Test.Framework.Test
chapter3TestGroup = testGroup "Chapter 3"
    [testGroup "Exercise 3.2: Merge-free insert"
        [testProperty "insert' works same as insert" prop_insertSame
        ]
    ,testGroup "drainHeap"
        [testProperty "results in sorted list" prop_drainHeapIsSorted
        ,testProperty "gets everything in heap" prop_drainHeapFindsAll
        ]
    ,testGroup "Exercise 3.3: fromList"
        [testCase "fromList is empty when list is" case_fromListPreservesEmpty
        ,testProperty "fromList min is list min" prop_fromListPreservesMin
        ,testProperty "fromList contains all elements" prop_fromListContainsAll
        ]
    ]

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all (\(x, y) -> x <= y) $ zip xs (tail xs)

prop_insertSame :: [Char] -> Bool
prop_insertSame xs = buildWith insert xs == buildWith insert' xs where
    buildWith ins = foldl (flip ins) E

prop_drainHeapIsSorted :: [Char] -> Bool
prop_drainHeapIsSorted xs = isSorted . drainHeap . foldl (flip insert) E $ xs

prop_drainHeapFindsAll :: [Int] -> Bool
prop_drainHeapFindsAll xs = (sort . drainHeap . foldl (flip insert) E $ xs) == sort xs

case_fromListPreservesEmpty :: Assertion
case_fromListPreservesEmpty = fromList ([] :: [Int]) @?= (empty :: LeftistHeap Int)

prop_fromListPreservesMin :: NonEmptyList Char -> Bool
prop_fromListPreservesMin (NonEmpty xs) = findMin (fromList xs) == Just (minimum xs)

prop_fromListContainsAll :: NonEmptyList Int -> Bool
prop_fromListContainsAll (NonEmpty xs) = (sort . drainHeap . fromList $ xs) == sort xs


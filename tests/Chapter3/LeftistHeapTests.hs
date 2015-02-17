module Chapter3.LeftistHeapTests (leftistHeapTestGroup) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck.Modifiers
import Data.List (sort)

import Chapter3.LeftistHeap

leftistHeapTestGroup :: Test.Framework.Test
leftistHeapTestGroup = testGroup "Chapter 3"
    [testGroup "Exercise 3.2: Merge-free insert"
        [testProperty "insert' works same as insert" prop_insertSame
        ]
    ,testGroup "drainHeap"
        [testProperty "results in sorted list" prop_drainHeapIsSorted
        ,testProperty "gets everything in heap" prop_drainHeapFindsAll
        ]
    ,testGroup "Exercise 3.3: fromList"
        [testCase "fromList is empty when list is" $ case_fromListPreservesEmpty fromList
        ,testProperty "fromList min is list min" $ prop_fromListPreservesMin fromList
        ,testProperty "fromList contains all elements" $ prop_fromListContainsAll fromList
        ]
    ,testGroup "Exercise 3.4: weight-based heaps"
        [testProperty "weight is actually weight" prop_weightIsNumElems
        ,testProperty "has weight-based leftist property" prop_WBLeftist
        ,testCase "fromListW is empty when list is" $ case_fromListPreservesEmpty fromListW
        ,testProperty "fromListW min is list min" $ prop_fromListPreservesMin fromListW
        ,testProperty "fromListW contains all elements" $ prop_fromListContainsAll fromListW
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

type FromListType a = [a] -> LeftistHeap a

case_fromListPreservesEmpty :: FromListType Int -> Assertion
case_fromListPreservesEmpty fl = fl ([] :: [Int]) @?= (empty :: LeftistHeap Int)

prop_fromListPreservesMin :: FromListType Char -> NonEmptyList Char -> Bool
prop_fromListPreservesMin fl (NonEmpty xs) = findMin (fl xs) == Just (minimum xs)

prop_fromListContainsAll :: FromListType Int -> NonEmptyList Int -> Bool
prop_fromListContainsAll fl (NonEmpty xs) = (sort . drainHeap . fl $ xs) == sort xs

prop_weightIsNumElems :: [Char] -> Bool
prop_weightIsNumElems xs = weight h == numElems h where
    h = foldl (flip insertW) emptyW xs
    numElems :: LeftistHeap a -> Int
    numElems E = 0
    numElems (T _ _ a b) = 1 + (numElems a) + (numElems b)

prop_WBLeftist :: NonEmptyList Int -> Bool
prop_WBLeftist (NonEmpty xs) = weight leftTree >= weight rightTree where
    (T _ _ leftTree rightTree) = fromListW xs

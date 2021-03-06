module Chapter3.BinomialHeapTests (binomialHeapTestGroup) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck
import Data.Function (on)
import Data.List (nubBy)

import Chapter3.BinomialHeap

binomialHeapTestGroup :: Test.Framework.Test
binomialHeapTestGroup = testGroup "Chapter 3 - BinomialHeaps"
    [testGroup "Basic binomial heap tests"
        [testProperty "stated rank matches actual rank" prop_rankIsTrue
        ,testProperty "findMin actually finds minimum" prop_findMinFindsMin
        ,testProperty "deleteMin increases minimum" prop_deleteMinIncreases
        ,testCase "simple link test" case_simpleLinkTest
        ,testProperty "insTree maintains rank order" prop_insTreeRankOrder
        ,testProperty "merge maintains rank order" prop_mergeRankOrder
        ,testProperty "insTree maintains unique tree rank" prop_insTreeUniqRank
        ,testProperty "merge maintains unique tree rank" prop_mergeUniqRank
        ,testProperty "findMin' acts like findMin" prop_findMin'
        ]
    ]

heapFromList :: (Ord a) => [a] -> Heap a
heapFromList = foldl insert []

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all (\(x, y) -> x <= y) $ zip xs (tail xs)

prop_rankIsTrue :: [Int] -> Bool
prop_rankIsTrue xs = map fst h == map (rank . snd) h where
    h = heapFromList xs

prop_findMinFindsMin :: NonEmptyList Int -> Bool
prop_findMinFindsMin (NonEmpty xs) = findMin (heapFromList xs) == Just (minimum xs)

heapsBiggerThan :: Int -> Gen (Heap Int)
heapsBiggerThan n = do
    xs <- arbitrary `suchThat` (\ys -> length ys >= n)
    return $ heapFromList xs

prop_deleteMinIncreases :: Property
prop_deleteMinIncreases = forAll (heapsBiggerThan 2) (\h -> findMin (deleteMin h) >= findMin h)

case_simpleLinkTest :: Assertion
case_simpleLinkTest = link (Node 1 [Node 2 []] :: Tree Int) (Node 3 [Node 4 []]) @?= (Node 1 [Node 3 [Node 4 []], Node 2 []])

hasRankOrder :: (Ord a) => Heap a -> Bool
hasRankOrder = isSorted . map fst

intTree :: Gen (Tree Int)
intTree = do
    h <- heapsBiggerThan 1
    i <- choose (0, length h - 1)
    return $ snd (h !! i)

prop_insTreeRankOrder :: [Int] -> Property
prop_insTreeRankOrder xs = forAll intTree (\t -> hasRankOrder (insTree h t)) where
    h = heapFromList xs

prop_mergeRankOrder :: [Int] -> [Int] -> Bool
prop_mergeRankOrder xs ys = hasRankOrder $ merge (heapFromList xs) (heapFromList ys)

hasUniqueRank :: (Ord a) => Heap a -> Bool
hasUniqueRank h = nubBy ((==) `on` fst) h == h

prop_insTreeUniqRank :: [Int] -> Property
prop_insTreeUniqRank xs = forAll intTree (\t -> hasUniqueRank (insTree h t)) where
    h = heapFromList xs

prop_mergeUniqRank :: [Int] -> [Int] -> Bool
prop_mergeUniqRank xs ys = hasUniqueRank $ merge (heapFromList xs) (heapFromList ys)

prop_findMin' :: [Int] -> Bool
prop_findMin' xs = findMin h == findMin' h where
    h = heapFromList xs

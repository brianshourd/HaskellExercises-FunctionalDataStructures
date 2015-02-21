module Chapter3.RedBlackTreeTests (redBlackTreeTestGroup) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck
--import Data.Function (on)
--import Data.List (nubBy)

import Chapter3.RedBlackTree

redBlackTreeTestGroup :: Test.Framework.Test
redBlackTreeTestGroup = testGroup "Chapter 3 - RedBlackTree"
    [testGroup "Some tests of test methods"
        [testCase "redInvariant pass" case_redInvariantPass
        ,testCase "redInvariant fail" case_redInvariantFail
        ,testCase "blackInvariant pass" case_blackInvariantPass
        ,testCase "blackInvariant fail" case_blackInvariantFail
        ]
    ,testGroup "Basic red black tree tests"
        [testProperty "insert preserves red invariant" prop_insertRedInvariant
        ,testProperty "insert preserves black invariant" prop_insertBlackInvariant
        ,testProperty "member finds all inserted elements" prop_memberFindsInserts
        ,testProperty "member doesn't find elements that weren't inserted" prop_memberNotFind
        ]
    ,testGroup "fromOrdList tests"
        [testProperty "preserves red invariant" prop_fromOrdListRedInvariant
        ,testProperty "preserves black invariant" prop_fromOrdListBlackInvariant
        ,testProperty "member finds all inserted elements" prop_fromOrdListMemberFindsInserts
        ]
    ,testGroup "insert' tests"
        [testProperty "insert' preserves red invariant" prop_insertRedInvariant'
        ,testProperty "insert' preserves black invariant" prop_insertBlackInvariant'
        ]
    ]

isBlackOrEmpty :: Tree a -> Bool
isBlackOrEmpty E = True
isBlackOrEmpty (T Black _ _ _) = True
isBlackOrEmpty _ = False

-- Invariant 1: No red nod has a red child
redInvariant :: Tree a -> Bool
redInvariant E = True
redInvariant (T Black t1 _ t2) = (redInvariant t1) && (redInvariant t2)
redInvariant (T Red t1 _ t2) = (isBlackOrEmpty t1) && (isBlackOrEmpty t2) && (redInvariant t1) && (redInvariant t2)

-- Invariant 2: Every path from the root to an empty node contains the same
-- number of black nodes
blackInvariant :: Tree a -> Bool
blackInvariant E = True
blackInvariant t = allTheSame . map countBlacks $ pathsToEmpty t where
    allTheSame [] = True
    allTheSame (x:xs) = all (== x) xs
    countBlacks = length . filter (== Black)
    pathsToEmpty E = []
    pathsToEmpty (T c t1 _ t2) = map (\cs -> c:cs) $ (pathsToEmpty t1) ++ (pathsToEmpty t2)

case_redInvariantPass :: Assertion
case_redInvariantPass = redInvariant t @?= True where
    t = T Red (T Black E 1 E) 2 (T Black E 3 (T Red E 7 E)) :: Tree Int

case_redInvariantFail :: Assertion
case_redInvariantFail = redInvariant t @?= False where
    t = T Red (T Black E 1 E) 2 (T Red E 2 E) :: Tree Int

case_blackInvariantPass :: Assertion
case_blackInvariantPass = blackInvariant t @?= True where
    t = T Black (T Red (T Black E 1 E) 1 (T Black E 1 E)) 1 (T Black E 1 E) :: Tree Int

case_blackInvariantFail :: Assertion
case_blackInvariantFail = blackInvariant t @?= True where
    t = T Black (T Red (T Black E 1 E) 1 E) 1 (T Black E 1 E) :: Tree Int

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldl insert E

prop_insertRedInvariant :: [Int] -> Bool
prop_insertRedInvariant = redInvariant . treeFromList

prop_insertBlackInvariant :: [Char] -> Bool
prop_insertBlackInvariant = blackInvariant . treeFromList

prop_memberFindsInserts :: [Int] -> Bool
prop_memberFindsInserts xs = all (member h) xs where
    h = treeFromList xs

prop_memberNotFind :: [Int] -> Property
prop_memberNotFind xs = forAll notInXs (\x -> member h x == False) where
    h = treeFromList xs
    notInXs = arbitrary `suchThat` (\x -> not $ x `elem` xs)

prop_fromOrdListRedInvariant :: OrderedList Int -> Bool
prop_fromOrdListRedInvariant (Ordered xs) = redInvariant . fromOrdList $ xs

prop_fromOrdListBlackInvariant :: OrderedList Char -> Bool
prop_fromOrdListBlackInvariant (Ordered xs) = blackInvariant . fromOrdList $ xs

prop_fromOrdListMemberFindsInserts :: OrderedList Int -> Bool
prop_fromOrdListMemberFindsInserts (Ordered xs) = all (member h) xs where
    h = fromOrdList xs

treeFromList' :: (Ord a) => [a] -> Tree a
treeFromList' = foldl insert' E

prop_insertRedInvariant' :: [Int] -> Bool
prop_insertRedInvariant' = redInvariant . treeFromList'

prop_insertBlackInvariant' :: [Char] -> Bool
prop_insertBlackInvariant' = blackInvariant . treeFromList'


{-# LANGUAGE OverloadedLists #-}
module Chapter5.DequeTests (dequeTestGroup) where

import Prelude hiding (head, tail, last, init, reverse)

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck

import Control.Monad (liftM)
import qualified Data.List as L
import GHC.Exts (IsList(..))
--import Data.Function (on)

import Chapter5.Deque

dequeTestGroup :: Test.Framework.Test
dequeTestGroup = testGroup "Chapter 5 - Double-ended queues"
    [testGroup "utility function tests"
        [testProperty "splitHalf lengths within 1" prop_splitHalfEven
        ,testProperty "splitHalf first is bigger" prop_splitHalfFirstBigger
        ,testProperty "splitHalf first return sorted" prop_splitHalfSorted
        ,testProperty "splitHalf second return reverse sorted" prop_splitHalfReversed
        ,testProperty "splitHalf first is beginning of input" prop_splitHalfBeginning
        ,testProperty "splitHalf second is beginning of reversed input" prop_splitHalfBeginningReversed
        ,testCase "splitHalf example" case_splitHalf1
        ,testCase "splitHalf single element" case_splitHalfSingle
        ]
    ,testGroup "Front of the Queue tests"
        [testCase "head empty gives nothing" case_headEmpty
        ,testProperty "head cons empty gives element" prop_headConsEmpty
        ,testProperty "tail is equal to deque before cons" prop_tailBeforeCons
        ,testProperty "draining with tail gets everything in order" prop_tailDrains
        ]
    ,testGroup "Back of the Queue tests"
        [testCase "last empty gives nothing" case_lastEmpty
        ,testProperty "last snoc empty gives element" prop_lastSnocEmpty
        ,testProperty "init is equal to deque before snoc" prop_initBeforeSnoc
        ,testProperty "draining with init gets everything reversed" prop_initDrains
        ]
    ,testGroup "Reflective properties of reversing"
        [testProperty "reverse actually reverses" prop_reverseReverses
        ,testProperty "cons is reverse snoc" prop_consSnoc
        ,testProperty "last is reverse head" prop_lastHead
        ,testProperty "init is reverse tail" prop_initTail
        ]
    ]

prop_splitHalfEven :: [Int] -> Bool
prop_splitHalfEven xs = abs ((length firstHalf) - (length secondHalf)) <= 1 where
    (firstHalf, secondHalf) = splitHalf xs

prop_splitHalfFirstBigger :: [Int] -> Bool
prop_splitHalfFirstBigger xs = (length firstHalf) >= (length secondHalf) where
    (firstHalf, secondHalf) = splitHalf xs

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all (\(x, y) -> x <= y) $ L.zip xs (L.tail xs)

prop_splitHalfSorted :: OrderedList Int -> Bool
prop_splitHalfSorted (Ordered xs) = isSorted firstHalf where
    (firstHalf, _) = splitHalf xs

prop_splitHalfReversed :: OrderedList Int -> Bool
prop_splitHalfReversed (Ordered xs) = isSorted (L.reverse secondHalf) where
    (_, secondHalf) = splitHalf xs

prop_splitHalfBeginning :: [Int] -> Bool
prop_splitHalfBeginning xs = firstHalf `L.isPrefixOf` xs where
    (firstHalf, _) = splitHalf xs

prop_splitHalfBeginningReversed :: [Int] -> Bool
prop_splitHalfBeginningReversed xs = secondHalf `L.isPrefixOf` (L.reverse xs) where
    (_, secondHalf) = splitHalf xs

case_splitHalf1 :: Assertion
case_splitHalf1 = splitHalf ([1..4] :: [Int]) @?= ([1,2], [4,3])

case_splitHalfSingle :: Assertion
case_splitHalfSingle = splitHalf ([1] :: [Int]) @?= ([1], [])

case_headEmpty :: Assertion
case_headEmpty = (head empty :: Maybe Char) @?= Nothing

prop_headConsEmpty :: Int -> Bool
prop_headConsEmpty x = head (cons x empty) == Just x

prop_tailBeforeCons :: [Int] -> [Int] -> Int -> Bool
prop_tailBeforeCons f r x = (tail . cons x $ d) == Just d where
    d = D f r

drainWithTail :: Deque a -> [a]
drainWithTail d' = L.unfoldr headTail d' where
    headTail d = do
        x <- head d
        xs <- tail d
        return (x, xs)

prop_tailDrains :: [Int] -> [Int] -> Bool
prop_tailDrains f r = drainWithTail d == toList d where
    d = D f r

case_lastEmpty :: Assertion
case_lastEmpty = (last empty :: Maybe Char) @?= Nothing

prop_lastSnocEmpty :: Int -> Bool
prop_lastSnocEmpty x = last (snoc x empty) == Just x

prop_initBeforeSnoc :: [Int] -> [Int] -> Int -> Bool
prop_initBeforeSnoc  f r x = (init . snoc x $ d) == Just d where
    d = D f r

drainWithInit :: Deque a -> [a]
drainWithInit d' = L.unfoldr lastInit d' where
    lastInit d = do
        x <- last d
        xs <- init d
        return (x, xs)

prop_initDrains :: [Int] -> [Int] -> Bool
prop_initDrains f r = (drainWithInit d) == (L.reverse . toList $ d) where
    d = D f r

reverse :: Deque a -> Deque a
reverse (D f l) = D l f

prop_reverseReverses :: [Int] -> [Int] -> Bool
prop_reverseReverses f l = (L.reverse . toList $ d) == (toList . reverse $ d) where
    d = D f l

prop_consSnoc :: [Int] -> [Int] -> Int -> Bool
prop_consSnoc f l x = (cons x d) == (reverse . snoc x . reverse $ d) where
    d = D f l

prop_lastHead :: [Int] -> [Int] -> Bool
prop_lastHead f l = (head d) == (last . reverse $ d) where
    d = D f l

prop_initTail :: [Int] -> [Int] -> Bool
prop_initTail f l = (tail d) == ((liftM reverse) . init . reverse $ d) where
    d = D f l


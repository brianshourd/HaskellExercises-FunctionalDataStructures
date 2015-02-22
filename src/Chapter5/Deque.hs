{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Chapter5.Deque where

import Prelude (undefined, (+), div)

import qualified Data.List as L

import Control.Monad (liftM)
import Data.Bool
import Data.Eq
import Data.Function
import Data.Maybe
import Data.Tuple (uncurry)
import GHC.Exts (IsList(..))
import Safe
import Text.Show (Show)

-- Exercise 5.1: Implement a double-ended queue
data Deque a = D [a] [a]
    deriving (Show)

instance IsList (Deque a) where
    type Item (Deque a) = a
    toList (D f l) = f L.++ (L.reverse l)
    fromList xs = D xs []

instance (Eq a) => Eq (Deque a) where
    (==) = (==) `on` toList

empty :: Deque a
empty = D [] []

isEmpty :: Deque a -> Bool
isEmpty (D [] []) = True
isEmpty _ = False

-- cons, head, and tail operate on the front of the deque
cons :: a -> Deque a -> Deque a
cons x (D f r) = D (x:f) r

head :: Deque a -> Maybe a
head (D [] r) = lastMay r
head (D f _) = headMay f

tail :: Deque a -> Maybe (Deque a)
tail (D f r) = case f of
    (_:xs) -> Just $ D xs r
    _ -> liftM ((uncurry . flip $ D) . splitHalf) $ initMay r

-- snoc, last, and init operate on the end of the deque
snoc :: a -> Deque a -> Deque a
snoc x (D f r) = D f (x:r)

last :: Deque a -> Maybe a
last (D f []) = lastMay f
last (D _ r) = headMay r

init :: Deque a -> Maybe (Deque a)
init (D f r) = case r of
    (_:xs) -> Just $ D f xs
    _ -> liftM ((uncurry D) . splitHalf) $ initMay f

-- Some utility functions
-- splitHalf splits its input in half, returning a pair. The first in the pair
-- is the first (and larger) half of the input. The second is the smaller half,
-- but reversed.
splitHalf :: [a] -> ([a], [a])
splitHalf xs = (firstHalf, L.reverse secondHalf) where
    (firstHalf, secondHalf) = L.splitAt midRight xs
    midRight = (L.length xs + 1) `div` 2


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Chapter2 where

-- Exercise 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : (suffixes $ tail xs)

-- Binary tree implementation
data Tree a = E
    | T (Tree a) a (Tree a)
    deriving (Show, Eq)

class Set s a | a -> s where
    empty :: s a
    insert :: s a -> a -> s a
    member :: s a -> a -> Bool

treeFromList :: (Set Tree a) => [a] -> Tree a
treeFromList = foldl insert empty

instance Ord a => Set Tree a where
    empty = E

    insert E x = T E x E
    insert s@(T l y r) x
        | x < y = T (insert l x) y r
        | x > y = T l y (insert r x)
        | otherwise = s

    member E _ = False
    member (T l y r) x
        | x < y = member l x
        | x > y = member r x
        | otherwise = True

-- Exercise 2.2
-- A slow implementation of depth
depth :: Tree a -> Int
depth E = 0
depth (T l _ r) = 1 + max (depth l) (depth r)

-- member' is similar to member, but also reports back the number of comparisons
-- it had to make
member' :: (Ord a) => Tree a -> a -> (Bool, Int)
member' E _ = (False, 0)
member' s z = member'impl s z 0 where
    member'impl E _ c = (False, c)
    member'impl (T l y r) x c
        | x < y = (resultl, countl + 1)
        | x > y = (resultr, countr + 2)
        | otherwise = (True, c + 2) where
            (resultl, countl) = member'impl l x c
            (resultr, countr) = member'impl r x c

-- member' is a better member, which takes no more than depth + 1 comparisons
betterMember' :: (Ord a) => Tree a -> a -> (Bool, Int)
betterMember' t' x' = betterMember'impl t' x' 0 Nothing where
    betterMember'impl E _ count Nothing = (False, count)
    betterMember'impl E x count (Just y) = (x == y, count + 1)
    betterMember'impl (T l y r) x count lastFalse
        | x < y = (resultl, countl + 1)
        | otherwise = betterMember'impl r x (count + 1) (Just y) where
            (resultl, countl) = betterMember'impl l x count lastFalse

-- Exercise 2.3
-- A better insertion method, that doesn't change when something which is
-- already in the set is added to the set.
-- The book says to do it with exceptions, which is certainly more efficient
-- than the method here. But I don't like using exceptions for controlling
-- program flow, so here is that with
betterInsert :: (Ord a) => Tree a -> a -> Tree a
betterInsert s' x' = case betterInsert' s' x' of
    Nothing -> s'
    Just t' -> t'

betterInsert' :: (Ord a) => Tree a -> a -> Maybe (Tree a)
betterInsert' E x = Just $ T E x E
betterInsert' (T l y r) x
    | x < y = betterInsert' l x >>= \l' -> Just $ T l' y r
    | x > y = betterInsert' r x >>= Just . T l y
    | otherwise = Nothing

-- Exercise 2.4
-- Better insert and better member for Set
-- Not actually going to do this, since there isn't much to do

-- Exercise 2.5
-- Part a - create a complete tree of depth d full of arbitrary elements
complete :: a -> Int -> Tree a
complete x d
    | d <= 0 = E
    | otherwise = T subTree x subTree where
        subTree = complete x (d - 1)

-- For testing, we need a size function
size :: Tree a -> Int
size E = 0
size (T l _ r) = 1 + (size l) + (size r)

-- Part b - create a tree of arbitrary elements of the given size
balancedTree :: a -> Int -> Tree a
balancedTree x s
    | s <= 0 = E
    | odd s = T (balancedTree x $ s `div` 2) x (balancedTree x $ s `div` 2)
    | otherwise = T (balancedTree x $ (s `div` 2) - 1) x (balancedTree x $ s `div` 2)

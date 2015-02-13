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

setFromList :: (Set Tree a) => [a] -> Tree a
setFromList = foldl insert empty

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

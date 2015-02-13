{-# LANGUAGE FlexibleInstances #-}
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

setFromList :: Set s a => [a] -> s a
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

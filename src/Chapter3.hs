{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Chapter3 where

-- Some code provided
class Heap h where
    empty     :: (Ord a) => h a
    isEmpty   :: (Ord a) => h a -> Bool
    insert    :: (Ord a) => a -> h a -> h a
    merge     :: (Ord a) => h a -> h a -> h a
    findMin   :: (Ord a) => h a -> Maybe a
    deleteMin :: (Ord a) => h a -> h a

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)

rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b = if rank a >= rank b then T (rank b + 1) x a b
              else T (rank a + 1) x b a

instance Heap LeftistHeap where
    empty = E
    isEmpty E = True
    isEmpty _ = False

    insert x h = merge (T 1 x E E) h

    merge h E = h
    merge E h = h
    merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
        if x <= y then makeT x a1 (merge b1 h2)
        else makeT y a2 (merge h1 b2)

    findMin E = Nothing
    findMin (T _ x _ _) = Just x

    deleteMin E = E
    deleteMin (T _ _ a b) = merge a b

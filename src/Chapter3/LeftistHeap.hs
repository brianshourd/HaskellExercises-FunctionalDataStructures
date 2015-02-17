{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Chapter3.LeftistHeap where

-- Some code provided
class Heap h where
    empty     :: (Ord a) => h a
    isEmpty   :: (Ord a) => h a -> Bool
    insert    :: (Ord a) => a -> h a -> h a
    merge     :: (Ord a) => h a -> h a -> h a
    findMin   :: (Ord a) => h a -> Maybe a
    deleteMin :: (Ord a) => h a -> h a

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)
    deriving (Show, Eq)

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

-- Exercise 3.2: Merge-free insert
insert' :: (Ord a) => a -> LeftistHeap a -> LeftistHeap a
insert' x E = T 1 x E E
insert' x h@(T _ y a b) =
    if x <= y then makeT x E h
    else makeT y a (insert x b)

-- Exercise 3.3 implement fromList
fromList :: (Ord a) => [a] -> LeftistHeap a
fromList = multiMerge . map (\x -> T 1 x E E) where
    multiMerge (h1:h2:hs) = merge (merge h1 h2) (multiMerge hs)
    multiMerge (h:hs) = merge h (multiMerge hs)
    multiMerge [] = empty

-- Used for testing
drainHeap :: (Ord a, Heap m) => m a -> [a]
drainHeap h = case (findMin h) of
    Nothing -> []
    Just x -> x:(drainHeap . deleteMin $ h) where

-- Exercise 3.4: Weight-based leftist heaps
-- We are re-purposing the LeftistHeap construct so that the Int represents the
-- weight, the number of elements in the heap. To go with this, we have the
-- class HeapW
class HeapW h where
    emptyW     :: (Ord a) => h a
    isEmptyW   :: (Ord a) => h a -> Bool
    insertW    :: (Ord a) => a -> h a -> h a
    mergeW     :: (Ord a) => h a -> h a -> h a
    findMinW   :: (Ord a) => h a -> Maybe a
    deleteMinW :: (Ord a) => h a -> h a

weight :: LeftistHeap a -> Int
weight E = 0
weight (T r _ _ _) = r

instance HeapW LeftistHeap where
    emptyW = E
    isEmptyW E = True
    isEmptyW _ = False

    insertW x h = mergeW (T 1 x E E) h

    mergeW h E = h
    mergeW E h = h
    mergeW h1@(T w1 x a1 b1) h2@(T w2 y a2 b2)
        | x <= y =
            if (weight b1) + w2 > (weight a1) then T (w1 + w2) x (mergeW b1 h2) a1
            else T (w1 + w2) x a1 (mergeW b1 h2)
        | otherwise =
            if (weight b2) + w1 > (weight a2) then T (w1 + w2) y (mergeW b2 h1) a2
            else T (w1 + w2) y a2 (mergeW b2 h1)

    findMinW E = Nothing
    findMinW (T _ x _ _) = Just x

    deleteMinW E = E
    deleteMinW (T _ _ a b) = mergeW a b

fromListW :: (Ord a) => [a] -> LeftistHeap a
fromListW = multiMerge . map (\x -> T 1 x E E) where
    multiMerge (h1:h2:hs) = mergeW (mergeW h1 h2) (multiMerge hs)
    multiMerge (h:hs) = mergeW h (multiMerge hs)
    multiMerge [] = emptyW

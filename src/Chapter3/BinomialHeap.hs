module Chapter3.BinomialHeap where

-- Get set up with the basics from the book
data Tree a = Node Int a [Tree a]
    deriving (Show, Eq)

root :: Tree a -> a
root (Node _ x _) = x

rank :: Tree a -> Int
rank (Node r _ _) = r

trees :: Tree a -> [Tree a]
trees (Node _ _ ts) = ts

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r1 x1 ts1) t2@(Node _ x2 ts2) =
    if (x1 <= x2) then Node (r1 + 1) x1 (t2:ts1)
    else Node (r1 + 1) x2 (t1:ts2)

type Heap a = [Tree a]

insTree :: (Ord a) => Heap a -> Tree a -> Heap a
insTree [] t = [t]
insTree h@(t:ts) t' =
    if rank t' < rank t then t' : h
    else insTree ts (link t t')

insert :: (Ord a) => Heap a -> a -> Heap a
insert h x = insTree h (Node 0 x [])

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h1 [] = h1
merge [] h2 = h2
merge h1@(t1:ts1) h2@(t2:ts2)
    | rank t1 < rank t2 = t1 : (merge ts1 h2)
    | rank t2 < rank t1 = t2 : (merge ts2 h1)
    | otherwise = insTree (merge ts1 ts2) (link t1 t2)

removeMinTree :: (Ord a) => Heap a -> Maybe (Tree a, Heap a)
removeMinTree [] = Nothing
removeMinTree (t:[]) = Just (t, [])
removeMinTree (t:ts) =
    if root t <= root t' then Just (t, ts)
    else Just (t', t:ts) where
        Just (t', _) = removeMinTree ts

findMin :: (Ord a) => Heap a -> Maybe a
findMin h = removeMinTree h >>= Just . root . fst

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin h = case removeMinTree h of
    Nothing -> []
    Just ((Node _ _ ts1), ts2) -> merge (reverse ts1) ts2

-- Exercise 3.5: define findMin without a call to removeMinTree
findMin' :: (Ord a) => Heap a -> Maybe a
findMin' [] = Nothing
findMin' h = Just . minimum . map root $ h

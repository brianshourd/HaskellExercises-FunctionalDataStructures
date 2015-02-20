module Chapter3.BinomialHeap where

-- Get set up with the basics from the book
-- Exercise 3.6 - reimplement without rank on each Tree
data Tree a = Node a [Tree a]
    deriving (Show, Eq)

root :: Tree a -> a
root (Node x _) = x

rank :: Tree a -> Int
rank (Node _ ts) = length ts

trees :: Tree a -> [Tree a]
trees (Node _ ts) = ts

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node x1 ts1) t2@(Node x2 ts2) =
    if (x1 <= x2) then Node x1 (t2:ts1)
    else Node x2 (t1:ts2)

type Heap a = [(Int, Tree a)]

insTree :: (Ord a) => Heap a -> Tree a -> Heap a
insTree [] t = [(rank t, t)]
insTree h@((r,t):ts) t' =
    if rank t' < r then (rank t',t') : h
    else insTree ts (link t t')

insert :: (Ord a) => Heap a -> a -> Heap a
insert h x = insTree h (Node x [])

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h1 [] = h1
merge [] h2 = h2
merge h1@(rt1@(r1,t1):ts1) h2@(rt2@(r2,t2):ts2)
    | r1 < r2 = rt1 : (merge ts1 h2)
    | r2 < r1 = rt2 : (merge ts2 h1)
    | otherwise = insTree (merge ts1 ts2) (link t1 t2)

removeMinTree :: (Ord a) => Heap a -> Maybe (Tree a, Heap a)
removeMinTree [] = Nothing
removeMinTree ((_,t):[]) = Just (t, [])
removeMinTree (rt@(r,t):ts) =
    if root t <= root t' then Just (t, ts)
    else Just (t', rt:ts) where
        Just (t', _) = removeMinTree ts

findMin :: (Ord a) => Heap a -> Maybe a
findMin h = removeMinTree h >>= Just . root . fst

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin h = case removeMinTree h of
    Nothing -> []
    Just ((Node _ ts1), ts2) -> merge (map (\t -> (rank t, t)) . reverse $ ts1) ts2

-- Exercise 3.5: define findMin without a call to removeMinTree
findMin' :: (Ord a) => Heap a -> Maybe a
findMin' [] = Nothing
findMin' h = Just . minimum . map (root . snd) $ h

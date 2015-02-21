module Chapter3.RedBlackTree where

-- Basics provided in text
data Color = Red
           | Black
    deriving (Show, Eq)
data Tree a = E
            | T Color (Tree a) a (Tree a)
    deriving (Show, Eq)

member :: (Ord a) => Tree a -> a -> Bool
member E _ = False
member (T _ t1 y t2) x
    | x < y = member t1 x
    | x > y = member t2 x
    | otherwise = True

insert :: (Ord a) => Tree a -> a -> Tree a
insert t' x' = T Black a' y' b' where
    (T _ a' y' b') = ins t' x'
    ins E x = T Red E x E
    ins s@(T c a y b) x
        | x < y = balance c (ins a x) y b
        | x > y = balance c a y (ins b x)
        | otherwise = s

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance Black (T Red a x (T Red b y c)) z d = balance' a b c d x y z
balance Black (T Red (T Red a x b) y c) z d = balance' a b c d x y z
balance Black a x (T Red b y (T Red c z d)) = balance' a b c d x y z
balance Black a x (T Red (T Red b y c) z d) = balance' a b c d x y z
balance color left val right = T color left val right

-- Helper function to avoid retyping so much in balance
balance' :: Tree a -> Tree a -> Tree a -> Tree a -> a -> a -> a -> Tree a
balance' a b c d x y z = T Red (T Black a x b) y (T Black c z d)

-- Exercise 3.9: Write a function fromOrdList that takes a sorted list with no
-- duplicated and converts it into a red-black tree in O(n) time
-- One way to achieve this is to build a tree where each level has the same
-- color and the colors alternate, ending with the final layer all Red
fromOrdList :: (Ord a) => [a] -> Tree a
fromOrdList ys = fst $ fromOrdList' ys (length ys)

-- fromOrdList' takes a source to pull inputs from and a max size to build
-- a tree to, and returns such a tree, along with the unused inputs
fromOrdList' :: [a] -> Int -> (Tree a, [a])
fromOrdList' [] _ = (E, [])
fromOrdList' xs 0 = (E, xs)
fromOrdList' (x:xs) 1 = (T Red E x E, xs)
fromOrdList' xs size = (T Black left x right, xs'') where
    (lsize, rsize) = split (size - 1)
    split s =
        if odd s then (half + 1, half)
        else (half, half) where
            half = s `div` 2
    (left, xs') = fromOrdList' xs lsize
    x = head xs'
    (right, xs'') = fromOrdList' (tail xs') rsize

-- Exercise 3.10 make balance even faster
rbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rbalance Black a x (T Red b y (T _ c z d)) = balance' a b c d x y z
rbalance Black a x (T Red (T _ b y c) z d) = balance' a b c d x y z
rbalance color left val right = T color left val right

lbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lbalance Black (T Red a x (T _ b y c)) z d = balance' a b c d x y z
lbalance Black (T Red (T _ a x b) y c) z d = balance' a b c d x y z
lbalance color left val right = T color left val right

insert' :: (Ord a) => Tree a -> a -> Tree a
insert' t' x' = T Black a' y' b' where
    (T _ a' y' b') = ins t' x'
    ins E x = T Red E x E
    ins s@(T c a y b) x
        | x < y = lbalance c (ins a x) y b
        | x > y = rbalance c a y (ins b x)
        | otherwise = s


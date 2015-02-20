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
fromOrdList = undefined

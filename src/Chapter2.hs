module Chapter2 where

-- Exercise 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : (suffixes $ tail xs)

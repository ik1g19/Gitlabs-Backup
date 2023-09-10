import Data.List

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

halve :: [a] -> ([a],a,[a])
halve xs = (take n xs, xs !! n, drop (n + 1) xs)
    where n = length xs `div` 2

balance :: [a] -> Tree a
balance [] = Leaf
balance xs = Node (balance ls) x (balance rs)
    where (ls, x, rs) = halve xs

toTree :: Ord a => [a] -> Tree a
toTree = balance . sort
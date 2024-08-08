--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A5 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (findBonding) where

-- Exercise A5

import Data.Maybe

data MTree a = RootNode [MTree a] | Node a [MTree a] deriving Show
--parameters are pair, number child took (1 indexed), list of other children
data Direction a = Children a Int [MTree a] | Root Int [MTree a] deriving Show
type Trail a = [Direction a]
type Zipper a = (MTree a, Trail a)

findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding p []                   = Just []
findBonding p xs | odd $ length xs = Nothing
                 | otherwise       = findBonds zipper
    where
        tree = buildTree xs p
        zipper = (tree,[])
        solutionDepth = ((length xs) `div` 2)
        fsn :: Zipper (a,a) -> Maybe (Zipper (a,a))
        fsn = findSoltNode solutionDepth 0 1
        findBonds :: Zipper (a,a) -> Maybe [(a,a)]
        findBonds z = (dupeReversePath . findSoltPath . fsn) z

buildTree :: [a] -> (a -> a -> Bool) -> MTree (a,a)
buildTree xs p = RootNode (generateChildren xs [] p)

--xs is the list of values that can be used for this row
--ys is the list of values that can be used by the children
--p is the predicate
--(x1:x2:[]) [] is for when all pairs have been made along this branch
--(x1:x2:[]) ys is for when all pairs have been made along this row
generateChildren :: [a] -> [a] -> (a -> a -> Bool) -> [MTree (a,a)]
generateChildren (x1:x2:[]) [] p | p x1 x2 && p x2 x1 = Node (x1,x2) [] : []
                                 | otherwise          = []
generateChildren (x1:x2:[]) ys p | p x1 x2 && p x2 x1 = Node (x1,x2) (generateChildren ys [] p) : []
                                 | otherwise          = []
generateChildren (x1:x2:xs) ys p | p x1 x2 && p x2 x1 = Node (x1,x2) (generateChildren (xs++ys) [] p) : generateChildren (x1:xs) (x2:ys) p
                                 | otherwise          = generateChildren (x1:xs) (x2:ys) p

--searches the tree for a solution
--sd is solution depth
--d is current depth
--nc is the child to take
findSoltNode :: Int -> Int -> Int -> Zipper (a,a) -> Maybe (Zipper (a,a))
findSoltNode sd d nc z@(Node _ [],(Children _ c _) : ds) | sd == d          = Just z
                                                         | otherwise        = findSoltNode sd (d-1) (c+1) (goUp z)
findSoltNode sd d nc z@(Node _ ts,(Children _ c _) : ds) | sd == d          = Just z
                                                         | (sd /= d) && 
                                                           (nc > length ts) = findSoltNode sd (d-1) (c+1) (goUp z)
                                                         | otherwise        = findSoltNode sd (d+1) 1 (goChild z nc)
findSoltNode sd d nc z@(Node _ ts,(Root c _) : ds)       | sd == d          = Just z
                                                         | (sd /= d) && 
                                                           (nc > length ts) = findSoltNode sd (d-1) (c+1) (goUp z)
                                                         | otherwise        = findSoltNode sd (d+1) 1 (goChild z nc)
findSoltNode sd d nc z@(RootNode ts,ds)                  | (nc > length ts) = Nothing
                                                         | otherwise        = findSoltNode sd (d+1) 1 (goChild z nc)

consOnMaybe :: (a,a) -> Maybe [(a,a)] -> Maybe [(a,a)]
consOnMaybe _ Nothing   = Nothing
consOnMaybe x (Just xs) = Just (x : xs)

findSoltPath :: Maybe (Zipper (a,a)) -> Maybe [(a,a)]
findSoltPath Nothing                                     = Nothing
findSoltPath ( Just z@(Node x _,(Children _ _ _) : ds) ) = x `consOnMaybe` findSoltPath (goUpMaybe (Just z))
    where
        --travel up tree when given node may exist
        goUpMaybe :: Maybe (Zipper (a,a)) -> Maybe (Zipper (a,a))
        goUpMaybe Nothing                             = Nothing
        goUpMaybe ( Just (t,(Root c ts) : ds) )       = Just (RootNode (insertAt t ts c),[])
        goUpMaybe ( Just (t,(Children x c ts) : ds) ) = Just (Node x (insertAt t ts c),ds)
findSoltPath ( Just z@(Node x _,(Root _ _) : ds) )       = Just (x : [])

dupeReversePath :: Maybe [(a,a)] -> Maybe [(a,a)]
dupeReversePath Nothing               = Nothing
dupeReversePath ( Just ((x1,x2):[]) ) = (x1,x2) `consOnMaybe` Just ((x2,x1) : [])
dupeReversePath ( Just ((x1,x2):xs) ) = (x1,x2) `consOnMaybe` ((x2,x1) `consOnMaybe` (dupeReversePath (Just xs)))

--insert into index of a list
--1 indexed
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys     1 = x:ys
insertAt x (y:ys) n = y:insertAt x ys (n-1)

--remove from index of a list
--0 indexed
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft,(_:rgt)) = splitAt idx xs

--branch down child
goChild :: Zipper (a,a) -> Int -> Zipper (a,a)
goChild (RootNode ts,ds) c = ((ts!!(c-1),(Root c (deleteAt (c-1) ts)):ds))
goChild (Node x ts,ds) c   = ((ts!!(c-1),(Children x c (deleteAt (c-1) ts)):ds))

--go up tree
goUp :: Zipper (a,a) -> Zipper (a,a)
goUp (t,(Root c ts) : ds)       = (RootNode (insertAt t ts c),[])
goUp (t,(Children x c ts) : ds) = (Node x (insertAt t ts c),ds)
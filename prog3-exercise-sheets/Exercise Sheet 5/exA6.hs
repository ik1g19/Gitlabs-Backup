{-# LANGUAGE DeriveGeneric #-}

--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A6 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq 

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)


-- Exercise A6
insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode v z@(Node l x c r,(L y _ _) : ts) | containsNode v (goRoot z) = goRoot z
                                                        | (v > x) && (v < y) = insertValue v (goUp z)
                                                        | otherwise = insertValue v (goRoot z)

insertFromCurrentNode v z@(Node l x c r,(R y _ _) : ts) | containsNode v (goRoot z) = goRoot z
                                                        | (v > y) && (v < x) = insertValue v (goUp z)
                                                        | otherwise = insertValue v (goRoot z)

insertFromCurrentNode v z                               | containsNode v (goRoot z) = goRoot z
                                                        | otherwise = insertValue v (goRoot z)

insertValue :: Ord a => a -> Zipper a -> Zipper a
insertValue v z@(Leaf,ts) = (Node Leaf v 1 Leaf,ts)
insertValue v z@(Node l x c r,ts) | v < x = insertValue v (goLeft z)
                                  | otherwise = insertValue v (goRight z)

--travels to root of the tree
goRoot :: Ord a => Zipper a -> Zipper a
goRoot (t,[]) = (t,[])
goRoot z = goRoot (goUp z)

--increments the visit counter of a tree
incrCnt :: Ord a => Zipper a -> Zipper a
incrCnt (Node l v c r,ts) = (Node l v (c+1) r,ts)
incrCnt (Leaf,ts) = (Leaf,ts)

containsNode :: Ord a => a -> Zipper a -> Bool
containsNode _ (Leaf,_) = False
containsNode y (Node l x c r,_) | x < y = containsNode y (r,[])
                                | x > y = containsNode y (l,[])
                                | otherwise = True

goLeft,goRight,goUp :: Ord a => Zipper a -> Zipper a
goLeft (Node l x c r,ts) = incrCnt (l,(L x c r):ts)
goRight (Node l x c r,ts) = incrCnt (r,(R x c l):ts)
goUp (t,(L x c r) : ts) = incrCnt (Node t x c r,ts)
goUp (t,(R x c l) : ts) = incrCnt (Node l x c t,ts)

mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])
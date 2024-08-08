{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A8 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (findMaxReducers,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Maybe Int]
type SMProg = [Instruction]

instance NFData (Instruction)

-- Exercise A8


findMaxReducers :: Stack -> [SMProg]
findMaxReducers []     = []
findMaxReducers (s:[]) = [[]]
findMaxReducers s      = breakLists [dynamicSolve s] []
    where
        --splits a list of lists into two different lists if a list contains more than one element
        --ps is previous elements
        dupeLists :: [[a]] -> [[a]] -> [[[a]]]
        dupeLists (x@(z:[]):[]) ps = [ps++[x]] --list contained no elements with more than one element
        dupeLists (x@(z:[]):xs) ps = dupeLists xs (ps++[x]) --list contains one element
        dupeLists ((z:zs):xs)   ps = [ps++[[z]]++xs,ps++[zs]++xs] --list contains more than one element

        breakLists :: [[[a]]] -> [[[a]]] -> [[a]]
        breakLists []     ps                                  = map fromSingletons ps
            where
                fromSingletons :: [[a]] -> [a]
                fromSingletons x = map (\(z:[]) -> z) x
        breakLists (x:xs) ps | sum (map length x) == length x = breakLists xs (ps++[x]) --if the list only contains singleton lists
                             | otherwise                      = breakLists (ps++(dupeLists x [])++xs) []


dynamicSolve :: Stack -> [[Instruction]]
dynamicSolve (s1:s2:[]) = instrs : []
    where instrs = map snd (findMax (s1:s2:[]) [])
dynamicSolve (s1:s2:ss) = instrs : dynamicSolve (val ++ ss)
    where
        --optimal value
        val = fst (head (findMax (s1:s2:[]) ss))
        --possible instructions to obtain it
        instrs = map snd (findMax (s1:s2:[]) ss)

--converts a list of maybe a to list of a
--ignores nothing values
maybeToList :: [Maybe a] -> [a]
maybeToList []           = []
maybeToList (Nothing:xs) = maybeToList xs
maybeToList (Just x:xs)  = x : maybeToList xs


--finds the optimal value to obtain between two numbers given the remaining stack
--returns optimal value and operation
--s is the stack containing the two values to compare
--rs is the rest of the stack
findMax :: Stack -> Stack -> [(Stack,Instruction)]
findMax s rs | odd (length (filter (<0) (maybeToList rs)))                                     --if odd number of negative numbers
                  = filter (\p -> (fstVal p) `elem` (maxAbs (map fstVal outputs))) outputs     --in rest of stack,choose highest absolute value

             | otherwise
                  = filter (\p -> fstVal p == maximum (map fstVal posOutputs)) posOutputs      --otherwise, choose highest value
    where
        --produces a list of possible operations, ignore any that evaluate to Nothing
        outputs :: [(Stack,Instruction)]
        outputs = filter (\p -> fst p /= [Nothing]) [(evalInst s [Add],Add),
                                                     (evalInst s [Sub],Sub),
                                                     (evalInst s [Mul],Mul),
                                                     (evalInst s [Div],Div),
                                                     (evalInst s [Pop],Pop)]

        --produces a list of outputs that evaluate to a positive answer
        posOutputs = filter (\p -> (0<=) (fstVal p)) outputs

        --extracts the value of the first elem of a outputs tuple
        fstVal :: (Stack,Instruction) -> Int
        fstVal ((Just z):[],_) = z

        --takes a list of ints and returns the ints with the largest absolute value
        maxAbs :: [Int] -> [Int]
        maxAbs zs = map fst (filter (\p -> snd p == max) (zip zs (map abs zs)))
            where max = maximum (map abs zs)



-- Exercise A7
evalInst :: Stack -> SMProg -> Stack
evalInst s p = last evals
    where evals = iterateStack p s

iterateStack :: SMProg -> Stack -> [Stack]
iterateStack []     x = x : []
iterateStack (i:is) x = x : iterateStack is (step i x)

--performs one instruction on the stack and returns
--the resulting stack
step :: Instruction -> Stack -> Stack
step _   []                       = error "Instruction applied to empty stack"
step Dup l@(x:xs)                 = x : l
step Pop (x:xs)                   = xs
step _   (x:[])                   = error "Instruction is binary operator but only one value is on stack"
step _   (Nothing:x:xs)           = Nothing : xs
step _   (x:Nothing:xs)           = Nothing : xs
step Add ((Just x1):(Just x2):xs) = Just (x1 + x2) : xs
step Sub ((Just x1):(Just x2):xs) = Just (x1 - x2) : xs
step Mul ((Just x1):(Just x2):xs) = Just (x1 * x2) : xs
step Div ((Just x1):(Just 0):xs)  = Nothing : xs
step Div ((Just x1):(Just x2):xs) = Just (x1 `div` x2) : xs
{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A9 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (isPossiblePower,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Maybe Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

-- Exercise A9


isPossiblePower :: Int -> Int -> Bool
isPossiblePower k l | k < 0 || l < 0 = False
                    | otherwise      = foldr (&&) True (map (check k l) [1..100])

check :: Int -> Int -> Int -> Bool
check k l x = recursiveCheck (x^k) l [Just x]

--t is the target
--l is the number of dups left
--s is the stack
recursiveCheck :: Int -> Int -> Stack -> Bool
recursiveCheck _ _ []                   = False
recursiveCheck t 0 s | (Just t:[]) == s = True
                     | otherwise        = recursiveCheck t 0 (evalInst s [Mul])
recursiveCheck t l s                    = recursiveCheck t l (evalInst s [Mul]) || recursiveCheck t (l-1) (evalInst s [Dup])



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
step _   []                       = []
step Dup l@(x:xs)                 = x : l
step Pop (x:xs)                   = xs
step _   (x:[])                   = []
step _   (Nothing:x:xs)           = Nothing : xs
step _   (x:Nothing:xs)           = Nothing : xs
step Add ((Just x1):(Just x2):xs) = Just (x1 + x2) : xs
step Sub ((Just x1):(Just x2):xs) = Just (x1 - x2) : xs
step Mul ((Just x1):(Just x2):xs) = Just (x1 * x2) : xs
step Div ((Just x1):(Just 0):xs)  = Nothing : xs
step Div ((Just x1):(Just x2):xs) = Just (x1 `div` x2) : xs
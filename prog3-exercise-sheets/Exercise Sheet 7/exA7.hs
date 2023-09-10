{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A7 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (evalInst,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Maybe Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

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
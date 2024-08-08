--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A4 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (neighbours) where

import Data.List
import Data.Ord

type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

-- Exercise A4

neighbours ::  Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs | k < 0 = error "k cannot be less than 0"
                    | otherwise = take k (sortBy (comparing (d p)) xs)
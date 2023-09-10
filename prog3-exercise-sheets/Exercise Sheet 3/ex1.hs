oddEvenSum :: Int
oddEvenSum = sum [x^2 | x <- [1,3..100]] + sum [y^3 | y <- [0,2..100]]
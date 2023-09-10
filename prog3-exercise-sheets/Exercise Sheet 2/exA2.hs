approxPi :: Int -> Double
approxPi n | n <= 0 = error "negative numbers or 0 cannot be used for approximations of pi"
           | otherwise = 2 * sum [product [1..a] / product [1,3..2 * a + 1] | a <- [0..fromIntegral n-1]]
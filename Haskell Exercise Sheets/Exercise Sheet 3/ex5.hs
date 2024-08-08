perfect :: Int -> [Int]
perfect n = [x | x <- [2..n], x == (sum . factors) x]
    where
    	factors :: Int -> [Int]
    	factors v = [x | x <- [1..v], v `mod` x == 0, x /= v]
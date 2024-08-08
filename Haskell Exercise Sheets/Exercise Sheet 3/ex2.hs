grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int,Int)]
square m = [(x,y) | x <- [0..m], y <- [0..m], x /= y]
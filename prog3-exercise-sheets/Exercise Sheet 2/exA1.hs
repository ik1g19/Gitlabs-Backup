-- --counts the number of values within three ranges
-- histogram :: Int -> [Int] -> [Int]
-- histogram n xs = [x,y,z]
--     where
--         x = length [a | a <- xs, a >= 0, a <= n - 1] --0 <= x <= n-1
--         y = length [a | a <- xs, a >= n, a <= 2 * n - 1] --n <= x <= 2n-1
--         z = length [a | a <- xs, a >= 2 * n, a <= 3 * n - 1] --2n <= x <= 3n-1


histogram :: Int -> [Int] -> [Int]
histogram n xs | n <= 0 = error "range sizes cannot be negative or 0"                           --given range is 0 or less
               | otherwise = [histogramCounter a | a <- [0..(maximum xs) `div` n]]              --given range is valid
    where
        histogramCounter :: Int -> Int
        histogramCounter range = length [a | a <- xs, a >= range * n, a <= (range + 1) * n - 1]
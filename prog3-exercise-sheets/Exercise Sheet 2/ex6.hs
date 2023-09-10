--checks four bank card numbers for errors
--returns true if valid, otherwise false
luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z | total `mod` 10 == 0 = True --if the answer is divisible by 10 it is valid
             | otherwise = False --otherwise invalid
    where
        --doubles a number and subtracts 9 if result > 9
        luhnDouble :: Int -> Int
        luhnDouble n | m > 9 = m - 9
                     | otherwise = m
            where m = 2 * n

        total = luhnDouble w + x + luhnDouble y + z
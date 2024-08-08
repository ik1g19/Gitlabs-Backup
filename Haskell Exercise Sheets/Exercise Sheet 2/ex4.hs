halve :: [a] -> ([a],[a])
halve xs | even listLength = splitAt (listLength `div` 2) xs
         | otherwise = ([],[])
    where listLength = length xs
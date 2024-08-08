import Debug.Trace

amSplit :: (Ord a, Show a) => [a] -> [[a]]
amSplit [] = []
amSplit xs = build [] [] xs
    where
        antiMonotone :: (Ord a, Show a) => [a] -> Bool
        antiMonotone ls = (antiMonotoneAsc ls) || (antiMonotoneDesc ls)
            where
                antiMonotoneAsc :: (Ord a, Show a) => [a] -> Bool
                antiMonotoneAsc [c] = True
                antiMonotoneAsc (c:cs) | head cs == c = antiMonotoneAsc cs
                                       | head cs > c = antiMonotoneDesc cs
                                       | otherwise = False

                antiMonotoneDesc :: (Ord a, Show a) => [a] -> Bool
                antiMonotoneDesc [c] = True
                antiMonotoneDesc (c:cs) | head cs == c = antiMonotoneDesc cs
                                        | head cs < c = antiMonotoneAsc cs
                                        | otherwise = False

        --accss is the accumulated list of anti monotone lists
        --cs is the current list that is anti monotone
        --d:ds is the list that is being searched
        build :: (Ord a, Show a) => [[a]] -> [a] -> [a] -> [[a]]
        build accss cs [] = accss ++ [cs]
        build accss cs (d:ds) | antiMonotone (cs ++ [d]) == True = build accss (cs ++ [d]) ds
                              | otherwise = build (accss ++ [cs]) [d] ds
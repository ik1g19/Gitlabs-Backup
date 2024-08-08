positions :: Eq a => a -> [a] -> [Int]
positions a as = find a $ zip as [0..]
    where
        find :: Eq a => a -> [ (a,b)] -> [b]
        find k t = [ v | (k',v) <- t, k==k']
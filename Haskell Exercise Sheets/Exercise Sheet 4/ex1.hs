all :: (a -> Bool) -> [a] -> Bool
all f xs = foldr (&&) True (map f xs)

any :: (a -> Bool) -> [a] -> Bool
any f xs = foldr (||) False (map f xs)
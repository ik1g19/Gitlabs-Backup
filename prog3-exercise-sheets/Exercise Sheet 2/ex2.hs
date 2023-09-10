fourth1 :: [a] -> a
fourth1 (w:x:y:z:xs) = z

fourth2 :: [a] -> a
fourth2 xs = xs !! 3

fourth3 :: [a] -> a
fourth3 ys = head (tail (tail (tail ys)))
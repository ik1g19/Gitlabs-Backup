safetail1 :: [a] -> [a]
safetail1 xs =
    if (length xs > 0) then tail xs
    else []

safetail2 :: [a] -> [a]
safetail2 xs | length xs > 0 = tail xs
             | otherwise = []

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs
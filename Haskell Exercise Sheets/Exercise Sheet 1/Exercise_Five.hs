quicktros [] = []
quicktros (x:xs) = quicktros rs ++ [x] ++ quicktros ls
                   where 
                     ls = [ a | a <- xs , a < x ]
                     rs = [ a | a <- xs , a > x ]
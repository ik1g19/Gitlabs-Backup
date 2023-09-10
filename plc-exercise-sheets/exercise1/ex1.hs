zipL :: ([Int],[Int]) -> [[Int]]
zipL (xs,ys) | length xs /= length ys = [[]]
zipL ([],[])     = []
zipL (x:xs,y:ys) = [x,y] : zipL (xs,ys)
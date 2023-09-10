{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where  

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random

import Control.Applicative
import Data.Maybe

instance NFData Orientation
instance NFData LamMacroExpr
instance NFData LamExpr

-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read,Generic)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read,Generic)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read,Generic)

-- END OF CODE YOU MUST NOT MODIFY

-- ADD YOUR OWN CODE HERE


{--| The following code is copyright of the University of Southampton |--
 --| The author of this code is Isaac Klugman                         |--}


------------------------------------------Challenge 1-------------------------------------------------          -- | inline comments


solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch ss css = map (findString css) ss


findString :: WordSearchGrid -> String -> (String,Maybe Placement)
findString css s = (s,findLocation css (0,0) s)


{--| recursively searches grid for first char of word |--
 --| returns Nothing or Placement                     |--}
findLocation :: WordSearchGrid -> Posn -> String -> Maybe Placement
findLocation css (x,y) s@(l:ls) | x > limit && y > limit                     = Nothing
                                | x > limit                                  = findLocation css (0,y+1) s
                                | elemAt css (x,y) == l && result /= Nothing = result
                                | otherwise                                  = findLocation css (x+1,y) s
    where
      result = findPlacement css (x,y) ls
      limit = length css - 1


{--| checks for hidden word in possible directions    |--}
findPlacement :: WordSearchGrid -> Posn -> String -> Maybe Placement
findPlacement css (x,y) s = findP dirs
    where
      findP []                                  = Nothing
      findP (d:ds) | checkWordDir css (x,y) s d = Just ( (x,y),d )
                   | otherwise                  = findP ds

      dirs = [Forward,Back,Up,Down,UpForward,UpBack,DownForward,DownBack]


checkWordDir :: WordSearchGrid -> Posn -> String -> Orientation -> Bool
checkWordDir css (x,y) (l:[]) dir | nextElem css (x,y) dir == Just l   = True
                                  | otherwise                          = False
checkWordDir css (x,y) (l:ls) dir | nextElem css (x,y) dir == Just l   = checkWordDir css (nextPos dir (x,y)) ls dir
                                  | otherwise                          = False


--------------------pattern matching for traversing the grid--------------------


{--| returns position of movement in a given direction |--}
nextPos :: Orientation -> Posn -> Posn
nextPos Forward     (x,y) = (x+1,y)
nextPos Back        (x,y) = (x-1,y)
nextPos Up          (x,y) = (x,y-1)
nextPos Down        (x,y) = (x,y+1)
nextPos UpForward   (x,y) = (x+1,y-1)
nextPos UpBack      (x,y) = (x-1,y-1)
nextPos DownForward (x,y) = (x+1,y+1)
nextPos DownBack    (x,y) = (x-1,y+1)


----------------------------Utility Functions-----------------------------------


elemAt :: [[a]] -> Posn -> a
elemAt ass (x,y) = (ass !! y) !! x


{--| returns specified adjacent element in grid,      |--
 --| relative to given position                       |--}
nextElem :: [[a]] -> Posn -> Orientation -> Maybe a
nextElem css (x,y) dir | x' < 0   || y' < 0  ||
                         x' > length css - 1 ||
                         y' > length css - 1    = Nothing
                       | otherwise              = Just (elemAt css (x',y'))
    where
      (x',y') = nextPos dir (x,y)


------------------------------------------Challenge 2-------------------------------------------------


{--| internal grid values are either a character
 --| or a placeholder for a random letter             |--}
data GridVal  = Letter Char | Rand deriving Eq
type RandGrid = [[GridVal]]


createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch ss den = do gen <- newStdGen                                                                   -- | initial generator
                             return (createGrid dim gen ss)
    where
      charInInput    = fromIntegral $ sum $ map length ss :: Double
      longestWordLen = fromIntegral $ foldl1 max $ map length ss :: Double
      dim            = floor $ head [x | x <- [0..], x^2 > (charInInput / den), x >= longestWordLen]            -- | calculates needed dimension of grid according to the density


createGrid :: Int -> StdGen -> [String] -> WordSearchGrid
createGrid dim gen ss = randToWord (charsFromStrs ss) gen' finalGrid
    where
      tempGrid         = replicate dim (replicate dim Rand)                                                     -- | fills grid with random values
      (finalGrid,gen') = addStrsToGrid tempGrid gen ss                                                          -- | final grid after all strings added

      charsFromStrs = rmdups . concat                                                                           -- | list of chars used in given strings


----------------------------Primary Functions-----------------------------------


randToWord :: [Char] -> StdGen -> RandGrid -> WordSearchGrid
randToWord cs gen []       = []
randToWord cs gen (row:rs) = let (newRow,newGen) = rowConvert cs gen row
                             in newRow : randToWord cs newGen rs


rowConvert :: [Char] -> StdGen -> [GridVal] -> ([Char],StdGen)
rowConvert cs gen []            = ([],gen)
rowConvert cs gen (Letter x:xs) = let (rows,gen') = rowConvert cs gen xs
                                  in (x : rows,gen')
rowConvert cs gen (Rand:xs)     = let (rows,gen') = rowConvert cs newGen xs
                                  in (randChar : rows,gen')
    where
      (index,newGen) = randomR (0,length cs - 1) gen
      randChar = cs !! index


{--| adds list of strings to given grid one by one    |--}
addStrsToGrid :: RandGrid -> StdGen -> [String] -> (RandGrid,StdGen)
addStrsToGrid rg gen (s:[]) = insertString rg s gen
addStrsToGrid rg gen (s:ss) = addStrsToGrid newGrid newGen ss
    where
     (newGrid,newGen) = insertString rg s gen


{--| takes a grid, string and a position              |--
 --| returns a list of valid orientations for the     |--
 --| string at that position                          |--}
validDirs :: RandGrid -> String -> Posn -> [Orientation]
validDirs rg s (x,y) = map fst $ filter ((True==) . snd) (zipF ( checkDir rg s (x,y) ) dirs)
    where dirs = [Forward,Back,Up,Down,UpForward,UpBack,DownForward,DownBack]


{--| applies given func to list and zips result with  |--
 --| original list                                    |--}
zipF :: (a -> b) -> [a] -> [(a,b)]
zipF f xs = zip xs $ map f xs


{--| checks whether an orientation for a string at a
 --| given position in a grid is valid                |--}
checkDir :: RandGrid -> String -> Posn -> Orientation -> Bool
checkDir rg s (x,y) dir | let (x',y') = posns !! (length s - 1),
                          x' < 0 || x' > length rg - 1 ||
                          y' < 0 || y' > length rg - 1                                                     = False
                        | foldl (&&) True (map (\(a,b) -> Letter a == b || b == Rand) $ zip s lettersGrid) = True
                        | otherwise                                                                        = False
    where
      posns = iterate (nextPos dir) (x,y)
      lettersGrid = take (length s) $ map (elemAt rg) posns


{--| adds an individual string to a given grid        |--
 --| returns new grid and new generator               |--}
insertString :: RandGrid -> String -> StdGen -> (RandGrid,StdGen)
insertString rg s gen | elemAt rg (x,y) /= Rand &&
                        elemAt rg (x,y) /= Letter (head s) = insertString rg s newGen                           -- | guard:if position is invalid, generate new position
                      | length vDirs == 0                  = insertString rg s newGen                           -- | guard:if no valid orientations exist, generate new position
                      | otherwise                          = (addToGrid randomDir s rg (x,y),newGen)
    where
      ( (x,y),newGen ) = generatePos gen (length rg)
      vDirs = validDirs rg s (x,y)
      randomDir = let (index,_) = randomR (0,length vDirs - 1) gen
                  in vDirs !! index

      addToGrid :: Orientation -> String -> RandGrid -> Posn -> RandGrid
      addToGrid dir (c:[]) rg (x',y') = insertAt2D (Letter c) (x',y') rg
      addToGrid dir (c:cs) rg (x',y') = addToGrid dir cs charAdded (nextPos dir (x',y'))
          where
            charAdded :: RandGrid
            charAdded = insertAt2D (Letter c) (x',y') rg


generatePos :: StdGen -> Int -> (Posn,StdGen)
generatePos gen dim = let (x,gen')  = randomR (0,dim - 1) gen  :: (Int,StdGen)
                          (y,gen'') = randomR (0,dim - 1) gen' :: (Int,StdGen)
                      in  ((x,y),gen'')


----------------------------Utility Functions-----------------------------------


{--| removes duplicates from a list                          |--
 --| code from https://stackoverflow.com/a/16109302/10218833 |--}
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort


{--| inserts element at location in 2d array          |--}
insertAt2D :: a -> (Int,Int) -> [[a]] -> [[a]]
insertAt2D newElement (x,y) grid | y == 0               = insertAt newElement x (grid !! y) : drop 1 belowRows
                                 | y == length grid - 1 = aboveRows ++ [insertAt newElement x (grid !! y)]
                                 | otherwise            = aboveRows ++ [insertAt newElement x (grid !! y)] ++ drop 1 belowRows
    where
      (aboveRows,belowRows) = splitAt y grid


{--| inserts element at given index of list                        |--
 --| using code from https://stackoverflow.com/a/43291593/10218833 |--}
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement : drop 1 as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as


------------------------------------------Challenge 3-------------------------------------------------


{--| Repeated for Clarity:                                                           |--
 --|                                                                                 |--
 --| data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read) |--
 --| data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |                      |--
 --|                LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)         |--}

prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef ms e) = macroDef ms ++ exprToStr ms e


----------------------------Primary Functions-----------------------------------


{--| converts bindings to strings                     |--}
macroDef :: [(String,LamExpr)] -> String
macroDef [] = ""
macroDef ( (name,expr):ms ) = foldl1 (++) ["def ",name," = ",exprToStr [] expr," in ",macroDef ms]


-- {--| replaces a macro with its definition             |--}
-- catchMacroStr :: [(String,LamExpr)] -> LamExpr -> String
-- catchMacro ms e | macros == [] = exprToStr ms e
--                 | otherwise    = fst $ head macros
--     where
--       macros = filter ( (e==) . snd ) ms

{--| replaces a macro with its definition             |--}
catchMacroExpr :: [(String,LamExpr)] -> LamExpr -> LamExpr
catchMacroExpr ms e | macros == [] = e
                    | otherwise    = LamMacro $ (fst . head) macros
    where
      macros = filter ( (e==) . snd ) ms

catchAndConvert :: [(String,LamExpr)] -> LamExpr -> String
catchAndConvert ms e = exprToStr ms $ catchMacroExpr ms e


{--| converts expr to str                             |--
 --| params:                                          |--
 --|   -list of macros and bindings                   |--
 --|   -expr to convert                               |--
 --| returns expr in string form                      |--}
exprToStr :: [(String,LamExpr)] -> LamExpr -> String
exprToStr ms e@(LamApp e1 e2) | eMacros == eNone  = none
                              | eMacros == eLeft  = left
                              | eMacros == eRight = right
                              | eMacros == eBoth  = both
                              | otherwise    = "fuck"
    where
      eMacros = LamApp (catchMacroExpr ms e1) (catchMacroExpr ms e2)                                            -- | when testing for parentheses, macros will be replaced
                                                                                                                -- | so macros must be replaced in the original expr

      none                   = foldl1 (++) [    catchAndConvert ms e1, " ", catchAndConvert ms e2    ]          -- | applying different uses of parenthese to
      left                   = foldl1 (++) ["(",catchAndConvert ms e1,") ", catchAndConvert ms e2    ]          -- | determine when they are necessary
      right                  = foldl1 (++) [    catchAndConvert ms e1, " (",catchAndConvert ms e2,")"]
      both                   = foldl1 (++) ["(",catchAndConvert ms e1,") (",catchAndConvert ms e2,")"]

      Just (LamDef _ eNone)  = parseLamMacro none
      Just (LamDef _ eLeft)  = parseLamMacro left
      Just (LamDef _ eRight) = parseLamMacro right
      Just (LamDef _ eBoth)  = parseLamMacro both

exprToStr ms (LamAbs   x  e ) = "\\x" ++ show x ++ " -> " ++ catchAndConvert ms e
exprToStr ms (LamVar   x    ) = "x"   ++ show x
exprToStr ms (LamMacro m    ) = m


------------------------------------------Challenge 4-------------------------------------------------


{--| Corresponding Grammar:                                                   |--
 --|                                                                          |--
 --| MacroExpr ::= "def" MacroName "=" Expr "in" MacroExpr | Expr             |--
 --| Expr ::=  Var | MacroName | Expr Expr | “\” Var “->” Expr | “(“ Expr “)” |--
 --| MacroName ::= UChar | UChar MacroName                                    |--
 --| UChar ::= "A" | "B" | ... | "Z"                                          |--
 --| Var ::= “x” Digits                                                       |--
 --| Digits ::= Digit | Digit Digits                                          |--
 --| Digit ::= “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”      |--}


parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro str | parsed == []                = Nothing
                  | foldl1 (&&) $                                                                               -- | if anything is left unparsed then the
                    map ( (""/=) . snd ) parsed = Nothing                                                       -- | parsing has failed
                  | otherwise                   = Just $ fstHead parsed
    where
      parsed = parse (macroExpr []) str


----------------------------Primary Functions-----------------------------------


macroExpr :: [ (String,LamExpr) ] -> Parser LamMacroExpr
macroExpr ms = do string "def"
                  name  <-  unique (map fst ms) (token macroName)
                  symbol "="
                  e     <- closedParse expr
                  token $ string "in"
                  macros <- macroExpr $ ms ++ [(name,e)]
                  return $ macros

           <|> do e <- token expr
                  return $ LamDef ms e


{--| parses an expr if it is closed                   |--}
closedParse :: Parser LamExpr -> Parser LamExpr
closedParse p = do e <- p
                   if closed e e then return e else empty


expr :: Parser LamExpr
expr = do terms <- some $ token term
          return $ foldl1 LamApp terms


term :: Parser LamExpr
term = do symbol "("
          e     <- expr
          symbol ")"
          return e

   <|> do symbol "\\"
          x     <- var
          symbol "->"
          e     <- expr
          return $ LamAbs x e

   <|> do {x <- var; return $ LamVar x}

   <|> do {name <- macroName; return $ LamMacro name}


macroName :: Parser String
macroName = do name <- some upper
               return name


var :: Parser Int
var = do char 'x'
         x <- nat
         return x


fstHead :: [(a,b)] -> a
fstHead = fst . head


----------------------------Utility Functions-----------------------------------


{--| parses an element only if it is unique to a      |--
 --| given list                                       |--}
unique :: Eq a => [a] -> Parser a -> Parser a
unique xs p = do x <- p
                 if x `elem` xs then empty else return x


{--| finds if an expression is closed                 |--}
closed :: LamExpr -> LamExpr -> Bool
closed expr (LamVar x)     = not $ freeVar x expr
closed expr (LamAbs _  e ) = closed expr e
closed expr (LamApp e1 e2) = closed expr e1 && closed expr e2

{--| finds if a given variable is free in an expr     |--}
freeVar :: Int -> LamExpr -> Bool
freeVar x (LamVar y)                 = x == y
freeVar x (LamAbs y e)   | x == y    = False
                         | otherwise = freeVar x e
freeVar x (LamApp e1 e2)             = freeVar x e1 || freeVar x e2


------------------------------------------Challenge 5-------------------------------------------------


{--| Repeated for Clarity:                                                           |--
 --|                                                                                 |--
 --| data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read) |--
 --| data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |                      |--
 --|                LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)         |--}


cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef ms e) = LamDef ms' e'
    where
      nextFreeInExpr  = if highestVar e == -1 then                                                              -- | the next free variable name in the expression
                          1
                        else
                          (+1) $ highestVar e
      nextFreeInMacro = if ms == [] then                                                                        -- | the next free variable name in the bindings
                          1
                        else
                          (+1) $ foldl1 max $ map (highestVar . snd) ms

      (ms',k) = cpsMacro [] ms $ max nextFreeInMacro nextFreeInExpr                                             -- | next free variable name is passed to converter
      (e',_)  = cpsExpr e k                                                                                     -- | for use in creating new variables


----------------------------Primary Functions-----------------------------------


{--| finds the variable name with the highest value   |--
 --| in an expression                                 |--}
highestVar :: LamExpr -> Int
highestVar (LamVar x) = x
highestVar (LamMacro _) = -1
highestVar (LamAbs x e) = max x $ highestVar e
highestVar (LamApp e1 e2) = max (highestVar e1) (highestVar e2)


{--| converts macro expr to cps form                  |--
 --| params:                                          |--
 --|   -converted macros                              |--
 --|   -macros to convert                             |--
 --|   -next available variable name                  |--
 --| returns the converted macro def                  |--}
cpsMacro :: [ (String,LamExpr) ] -> [ (String,LamExpr) ] -> Int -> ([ (String,LamExpr) ],Int)
cpsMacro es' [] k = (es',k)
cpsMacro es' ( (mName,mExpr):es ) k = cpsMacro (es'++[(mName,e')]) es k'
    where (e',k') = cpsExpr mExpr k


{--| converts a lambda expression to cps form         |--
 --| params:                                          |--
 --|   -expr to convert                               |--
 --|   -next available variable name                  |--
 --| returns pair of converted expr and next          |--
 --| available variable name                          |--}
cpsExpr :: LamExpr -> Int -> (LamExpr,Int)
cpsExpr (LamVar x) k     = (LamAbs k $ LamApp (LamVar k) (LamVar x),k+1)

cpsExpr (LamAbs x e) k   = (LamAbs k $ LamApp (LamVar k) $ LamAbs x e',k')
    where (e',k') = cpsExpr e (k+1)

cpsExpr (LamApp e1 e2) k = (LamAbs k $ LamApp e1' $
                            LamAbs f $ LamApp e2' $
                            LamAbs e $ foldl1 LamApp $ [LamVar f,LamVar e,LamVar k],k'')
    where
      f = k+1
      e = k+2
      (e1',k')  = cpsExpr e1 (e+1)
      (e2',k'') = cpsExpr e2 k'

cpsExpr (LamMacro name) k = (LamMacro name,k)


exId =  (LamAbs 1 (LamVar 1))


-- Challenge 6


{--| Repeated for Reference:                                             |--
 --|                                                                     |--
 --| freeVar :: Int -> LamExpr -> Bool                                   |--
 --| freeVar x (LamVar y)                 = not $ x == y                 |--
 --| freeVar x (LamAbs y e)   | x == y    = False                        |--
 --|                          | otherwise = freeVar x e                  |--
 --| freeVar x (LamApp e1 e2)             = freeVar x e1 || freeVar x e2 |--}


compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter e b = (innerSteps,outerSteps,innerCps,outerCps)
    where
      innerSteps = let reduces = trace innerRedn1 b e
                       tillNoRedex = takeWhile (Nothing/=) reduces
                   in if Nothing `elem` reduces then Just $ length tillNoRedex else Nothing

      outerSteps = let reduces = trace outerRedn1 b e
                       tillNoRedex = takeWhile (Nothing/=) reduces
                   in if Nothing `elem` reduces then Just $ length tillNoRedex else Nothing

      innerCps   = let LamDef ms' e' = cpsTransform e
                       reduces = trace innerRedn1 b $ LamDef ms' $ LamApp e' exId
                       tillNoRedex = takeWhile (Nothing/=) reduces
                   in if Nothing `elem` reduces then Just $ length tillNoRedex else Nothing

      outerCps   = let LamDef ms' e' = cpsTransform e
                       reduces = trace outerRedn1 b $ LamDef ms' $ LamApp e' exId
                       tillNoRedex = takeWhile (Nothing/=) reduces
                   in if Nothing `elem` reduces then Just $ length tillNoRedex else Nothing


innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 (LamDef ms e) | redex e   = Just $ LamDef ms e'
                         | otherwise = Nothing
    where e' = eval1cbv ms e


outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef ms e) | redex e   = Just $ LamDef ms e'
                         | otherwise = Nothing
    where e' = eval1cbn ms e


----------------------------Primary Functions-----------------------------------


{--| substitutes an expression into another expr      |--
 --| params:                                          |--
 --|   -expr to sub in to                             |--
 --|   -variable being replaced                       |--
 --|   -expr to sub in                                |--
 --| returns new expr                                 |--}
subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y    = e
                     | otherwise = LamVar x

subst (LamAbs x e) y e' | x /= y && not (freeVar x e')    = LamAbs x $ subst e y e'
                        | x /= y && freeVar x e'          = let x' = rename x e in
                                                            subst (LamAbs x' $ subst e x $ LamVar x') y e'
                        | x == y                          = LamAbs x e

subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

rename :: Int -> LamExpr -> Int
rename x e = highestVar e + 1


{--| returns true if an expression contains a redex   |--}
redex :: LamExpr -> Bool
redex (LamAbs _ _)            = False
redex (LamVar _)              = False
redex (LamMacro _)            = True
redex (LamApp (LamAbs _ _) _) = True
redex (LamApp e1 e2)          = redex e1 || redex e2


eval1cbv :: [ (String,LamExpr) ] -> LamExpr -> LamExpr
eval1cbv ms (LamAbs x e)                           = LamAbs x e
eval1cbv ms (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
eval1cbv ms (LamApp e1@(LamAbs x e) e2)            = LamApp e1 (eval1cbv ms e2)
eval1cbv ms (LamApp e1 e2)                         = LamApp (eval1cbv ms e1) e2
eval1cbv ms (LamMacro name)                        = (snd . head) $ filter ((name==) . fst) ms
eval1cbv ms (LamVar x)                             = LamVar x


eval1cbn :: [ (String,LamExpr) ] -> LamExpr -> LamExpr
eval1cbn ms (LamAbs x e)              = LamAbs x e
eval1cbn ms (LamMacro name)           = (snd . head) $ filter ((name==) . fst) ms
eval1cbn ms (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbn ms (LamApp e1 e2)            = LamApp (eval1cbn ms e1) e2


{--| returns the trace of reductions for an
 --| expression                                       |--
 --| params:                                          |--
 --|   -single step reduction strat                   |--
 --|   -bound for reductions                          |--
 --|   -expr to be reduced                            |--}
trace :: (LamMacroExpr -> Maybe LamMacroExpr) -> Int -> LamMacroExpr -> [Maybe LamMacroExpr]
trace ssev b e = take b $ reductions ssev e

reductions :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> [Maybe LamMacroExpr]
reductions ssev e = drop 1 evals
    where
      evals :: [Maybe LamMacroExpr]
      evals = iterate (>>=ssev) $ Just e
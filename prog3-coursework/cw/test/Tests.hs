import Challenges
import Data.List


-------------------------------------Main---------------------------------------


main :: IO ()
main = do challenge1Test
          challenge2Test
          challenge3Test
          challenge4Test
          challenge5Test
          challenge6Test


-----------------------------Challenge 1 Testing--------------------------------


challenge1Test :: IO ()
challenge1Test = do putStrLn "========================================="
                    putStrLn "Challenge 1 Start Test"
                    putStrLn "=========================================\n"
                    assert1 (exGrid1'1,exWords1'1) (solveWordSearch exWords1'1 exGrid1'1) exAns1'1
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert1 (exGrid1'2,exWords1'2) (solveWordSearch exWords1'2 exGrid1'2) exAns1'2
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert1 (exGrid1'3,exWords1'3) (solveWordSearch exWords1'3 exGrid1'3) exAns1'3
                    putStrLn "\n========================================="
                    putStrLn "Challenge 1 End Test"
                    putStrLn "=========================================\n"


assert1 :: (WordSearchGrid,[String]) -> [ (String,Maybe Placement) ] -> [ (String,Maybe Placement) ] -> IO ()
assert1 (grid,words) result correct = do putStrLn "Testing Grid:\n"
                                         printGrid grid
                                         putStrLn "\nWords Hidden are:"
                                         pPrintList 5 words
                                         putStrLn "\nExpected Result:"
                                         pPrintList 3 correct
                                         lineBreak
                                         if result == correct then
                                           putStrLn "Passed! Result:"
                                         else
                                           putStrLn "Failed! Result:"
                                         pPrintList 3 result


-----------------------------Challenge 2 Testing--------------------------------


challenge2Test :: IO ()
challenge2Test = do putStrLn "========================================="
                    putStrLn "Challenge 2 Start Test"
                    putStrLn "=========================================\n"
                    (g1,sol1) <- createAndSolve ["HIDE","THIS","WORD"] 0.3
                    assert2 1 g1 0.3 sol1
                    (g2,sol2) <- createAndSolve ["LONGER","WORD","SEARCH","TO","SOVLE"] 0.4
                    assert2 2 g2 0.4 sol2
                    (g3,sol3) <- createAndSolve ["THIS","ONE","$HAS$","+SYMBOLS+","AS","WELL"] 0.6
                    assert2 3 g3 0.6 sol3
                    putStrLn "========================================="
                    putStrLn "Challenge 2 End Test"
                    putStrLn "=========================================\n"


assert2 :: Int -> WordSearchGrid -> Double -> [ (String,Maybe Placement) ] -> IO ()
assert2 gridNumber grid density answers = do putStrLn "- - - - - - - - - - - - - - - - - -"
                                             putStrLn $ "Checking Grid Number: " ++ show gridNumber
                                             putStrLn "- - - - - - - - - - - - - - - - - -\n"
                                             printGrid grid
                                             putStrLn "\nShould Contain these Words:"
                                             pPrintList 3 answers
                                             lineBreak
                                             wordsPresent grid answers
                                             lineBreak
                                             densityCheck grid density answers
                                             lineBreak
                          
wordsPresent :: WordSearchGrid -> [ (String,Maybe Placement) ] -> IO ()
wordsPresent grid answers = do putStrLn "- - - - - - - - - - - - - - - - - -"
                               putStrLn "Testing Words are Present"
                               putStrLn "- - - - - - - - - - - - - - - - - -\n"
                               if foldl1 (||) $ map ((Nothing/=) . snd) answers then
                                 putStrLn "Passed Test! All words are present!"
                               else
                                 do putStrLn "Failed Test! Not all words are present"
                                    putStrLn "List of words that weren't found:"
                                    pPrintList 3 $ map fst $ filter ((Nothing/=) . snd) answers

densityCheck :: WordSearchGrid -> Double -> [ (String,Maybe Placement) ] -> IO ()
densityCheck grid density answers = do putStrLn "- - - - - - - - - - - - - - - - - -"
                                       putStrLn "Testing Desnity is Correct"
                                       putStrLn "- - - - - - - - - - - - - - - - - -\n"
                                       let charPositions = map ( \(word,Just ( posn,dir )) -> take (length word)        -- | words can overlap so finds position of each
                                                           $ iterate (nextPos dir) posn ) answers                       -- | char of each word
                                       let hiddenPositions = foldl1 (++) charPositions                                  -- | and only uses unique positions to find density
                                       let uniquePositions = rmdups hiddenPositions
                                       let d = (fromIntegral $ length uniquePositions) /
                                               (fromIntegral $ sum $ map length grid)
                                       if d < density then
                                         putStrLn "Passed Test!"
                                       else
                                         putStrLn "Failed Test!"
                                       putStrLn $ foldl1 (++) ["Desired density was ",show density,
                                                               " and the density of the generated grid was ",show d]

createAndSolve :: [ String ] -> Double -> IO (WordSearchGrid,[ (String,Maybe Placement) ])
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       return (g,soln)

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

{--| removes duplicates from a list                          |--
 --| code from https://stackoverflow.com/a/16109302/10218833 |--}
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-----------------------------Challenge 3 Testing--------------------------------


assert3 :: LamMacroExpr -> String -> String -> IO ()
assert3 macro expected result = do putStrLn "Pretty Printing:"
                                   putStrLn $ show macro ++ "\n"
                                   putStrLn "Expected Result:"
                                   putStrLn $ expected ++ "\n"
                                   if expected == result then
                                     putStrLn "Passed! Result:"
                                   else
                                     putStrLn "Failed! Result:"
                                   putStrLn result

challenge3Test :: IO ()
challenge3Test = do putStrLn "========================================="
                    putStrLn "Challenge 3 Start Test"
                    putStrLn "=========================================\n"
                    assert3 ex3'1 ex3'1Ans (prettyPrint ex3'1)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert3 ex3'2 ex3'2Ans (prettyPrint ex3'2)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert3 ex3'3 ex3'3Ans (prettyPrint ex3'3)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert3 ex3'4 ex3'4Ans (prettyPrint ex3'4)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert3 ex3'5 ex3'5Ans (prettyPrint ex3'5)
                    putStrLn "\n========================================="
                    putStrLn "Challenge 3 End Test"
                    putStrLn "=========================================\n"


-----------------------------Challenge 4 Testing--------------------------------


assert4 :: String -> Maybe LamMacroExpr -> Maybe LamMacroExpr -> IO ()
assert4 macro expected result = do putStrLn "Parsing:"
                                   putStrLn $ macro ++ "\n"
                                   putStrLn "Expected Result:"
                                   putStrLn $ show expected ++ "\n"
                                   if expected == result then
                                     putStrLn "Passed! Result:"
                                   else
                                     putStrLn "Failed! Result:"
                                   putStrLn $ show result

challenge4Test :: IO ()
challenge4Test = do putStrLn "========================================="
                    putStrLn "Challenge 4 Start Test"
                    putStrLn "=========================================\n"
                    assert4 ex4'1 ex4'1Ans (parseLamMacro ex4'1)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert4 ex4'2 ex4'2Ans (parseLamMacro ex4'2)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert4 ex4'3 ex4'3Ans (parseLamMacro ex4'3)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert4 ex4'4 ex4'4Ans (parseLamMacro ex4'4)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert4 ex4'5 ex4'5Ans (parseLamMacro ex4'5)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert4 ex4'6 ex4'6Ans (parseLamMacro ex4'6)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert4 ex4'7 ex4'7Ans (parseLamMacro ex4'7)
                    putStrLn "\n========================================="
                    putStrLn "Challenge 4 End Test"
                    putStrLn "=========================================\n"


-----------------------------Challenge 5 Testing--------------------------------


assert5 :: LamMacroExpr -> LamMacroExpr -> LamMacroExpr -> IO ()
assert5 toTest expected result = do putStrLn "CPS Translating:"
                                    putStrLn $ prettyPrint toTest ++ "\n"
                                    putStrLn "(Actual Internal Value):"
                                    putStrLn $ show toTest ++ "\n"
                                    putStrLn "Expected Result:"
                                    putStrLn $ show expected ++ "\n"
                                    if expected == result then
                                      putStrLn "Passed! Result:"
                                    else
                                      putStrLn "Failed! Result:"
                                    putStrLn $ show result

challenge5Test :: IO ()
challenge5Test = do putStrLn "========================================="
                    putStrLn "Challenge 5 Start Test"
                    putStrLn "=========================================\n"
                    assert5 ex5'1 ex5'1Ans (cpsTransform ex5'1)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert5 ex5'2 ex5'2Ans (cpsTransform ex5'2)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert5 ex5'3 ex5'3Ans (cpsTransform ex5'3)
                    putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    assert5 ex5'4 ex5'4Ans (cpsTransform ex5'4)
                    putStrLn "\n========================================="
                    putStrLn "Challenge 5 End Test"
                    putStrLn "=========================================\n"


-----------------------------Challenge 6 Testing--------------------------------


assert6Inner :: LamMacroExpr -> Maybe LamMacroExpr -> Maybe LamMacroExpr -> IO ()
assert6Inner toTest expected result = do putStrLn "Testing Inner one step Reduction of:"
                                         putStrLn $ prettyPrint toTest ++ "\n"
                                         putStrLn "(Actual Internal Value):"
                                         putStrLn $ show toTest ++ "\n"
                                         putStrLn "Expected Result:"
                                         putStrLn $ show expected ++ "\n"
                                         if expected == result then
                                           putStrLn "Passed! Result:"
                                         else
                                           putStrLn "Failed! Result:"
                                         putStrLn $ show result

assert6Outer :: LamMacroExpr -> Maybe LamMacroExpr -> Maybe LamMacroExpr -> IO ()
assert6Outer toTest expected result = do putStrLn "Testing Outer one step Reduction of:"
                                         putStrLn $ prettyPrint toTest ++ "\n"
                                         putStrLn "(Actual Internal Value):"
                                         putStrLn $ show toTest ++ "\n"
                                         putStrLn "Expected Result:"
                                         putStrLn $ show expected ++ "\n"
                                         if expected == result then
                                           putStrLn "Passed! Result:"
                                         else
                                           putStrLn "Failed! Result:"
                                         putStrLn $ show result

assert6Compare :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int) -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int) -> IO ()
assert6Compare toTest bound expected result = do putStrLn "Testing Comparisons of Reductions of:"
                                                 putStrLn $ prettyPrint toTest ++ "\n"
                                                 putStrLn $ "For a Bound of: " ++ show bound ++ "\n"
                                                 putStrLn "(Actual Internal Value):"
                                                 putStrLn $ show toTest ++ "\n"
                                                 putStrLn "Expected Result:"
                                                 putStrLn $ show expected ++ "\n"
                                                 if expected == result then
                                                   putStrLn "Passed! Result:"
                                                 else
                                                   putStrLn "Failed! Result:"
                                                 putStrLn $ show result

challenge6Test :: IO ()
challenge6Test = do putStrLn "========================================="
                    putStrLn "Challenge 6 Start Test"
                    putStrLn "=========================================\n"
                    -- putStrLn "\n- - - - - - - - - - - - - - - - - -"
                    -- putStrLn "Testing Inner Reduction"
                    -- putStrLn "- - - - - - - - - - - - - - - - - -\n"
                    -- assert6Inner ex5'1 ex5'1Ans (cpsTransform ex5'1)
                    -- putStrLn "\n- - - - - - - - - - - - - - - - - -\n"
                    -- putStrLn "\n- - - - - - - - - - - - - - - - - -"
                    -- putStrLn "Testing Outer Reduction"
                    -- putStrLn "- - - - - - - - - - - - - - - - - -\n"
                    putStrLn "- - - - - - - - - - - - - - - - - -"
                    putStrLn "Testing Comparisons of Reductions"
                    putStrLn "- - - - - - - - - - - - - - - - - -\n"
                    assert6Compare ex6'1 10 ex6'1Ans (compareInnerOuter ex6'1 10)
                    putStrLn "- - - - - - - - - - - - - - - - - -\n"
                    assert6Compare ex6'2 10 ex6'2Ans (compareInnerOuter ex6'2 10)
                    putStrLn "- - - - - - - - - - - - - - - - - -\n"
                    assert6Compare ex6'3 10 ex6'3Ans (compareInnerOuter ex6'3 10)
                    putStrLn "- - - - - - - - - - - - - - - - - -\n"
                    assert6Compare ex6'4 100 ex6'4Ans (compareInnerOuter ex6'4 10)
                    -- putStrLn "- - - - - - - - - - - - - - - - - -\n"
                    -- assert6Compare ex6'5 30 ex6'5Ans (compareInnerOuter ex6'5 10)
                    -- putStrLn "- - - - - - - - - - - - - - - - - -\n"
                    -- assert6Compare ex6'6 30 ex6'6Ans (compareInnerOuter ex6'6 10)
                    -- putStrLn "- - - - - - - - - - - - - - - - - -\n"
                    -- assert6Compare ex6'7 1000 ex6'7Ans (compareInnerOuter ex6'7 10)
                    putStrLn "========================================="
                    putStrLn "Challenge 6 End Test"
                    putStrLn "=========================================\n"


------------------------------Printing Functions--------------------------------


printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn $ intersperse ' ' w
                      printGrid ws

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

pPrintList :: Show a => Int -> [a] -> IO ()
pPrintList val xs = do putStr "["
                       let (first:rest) = splitEvery val xs
                       putStr $ (init . tail) $ show first
                       printRest rest
    where
        printRest :: Show a => [[a]] -> IO ()
        printRest []     = putStr $ "]\n"
        printRest (x:xs) = do putStr $ (++) "\n " $ (init . tail) $ show x
                              printRest xs

zipF :: (a -> b) -> [a] -> [(a,b)]
zipF f xs = zip xs $ map f xs

lineBreak :: IO ()
lineBreak = putStrLn ""


-------------------------------------Examples-----------------------------------


exGrid1'1  = [ "HAGNIRTSH",
               "SACAGETAK",
               "GCSTACKEL",
               "MGHKMILKI",
               "EKNLETGCN",
               "TNIRTLETE",
               "IRAAHCLSR",
               "MAMROSAGD",
               "GIZKDDNRG" ]
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exAns1'1   = [("HASKELL",Just((0,0),DownForward)),("STRING",Just((7,0),Back)),("STACK",Just((2,2),Forward)),
              ("MAIN",   Just((2,7),Up         )),("METHOD",Just((4,3),Down))]

exGrid1'2  = ["ROBREUMBR",
              "AURPEPSAN",
              "UNLALMSEE",
              "YGAUNPYYP",
              "NLMNBGENA",
              "NBLEALEOR",
              "ALRYPBBLG",
              "NREPBEBEP",
              "YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]

exAns1'2   = [("BANANA",   Just((5,6),UpBack  )),("ORANGE",Just((1,0),DownForward)),("MELON",Just((7,8),Up      )),
              ("RASPBERRY",Just((8,0),DownBack)),("APPLE", Just((2,8),UpForward  )),("PLUM", Just((5,1),DownBack)),
              ("GRAPE",    Just((8,6),Up      ))]

exGrid1'3  = ["CNOGLNCMGNT",
              "EIATR+ECATJ",
              "RCVNCVJCCAM",
              "JLNSAINRVH+",
              "HMPENINAVMY",
              "UAIPICLJVJH",
              "+RNYSCLUMIT",
              "NGNTCLSRJPU",
              "NOYHPIP+GLH",
              "RRVOHRT+GOT",
              "JPUNVGSCIEA"]
exWords1'3 = ["C++","JAVA","PYTHON","PROGRAM","JULIAN","SCIENCE"]

exAns1'3   = [("C++",    Just ((7,10),Up)),("JAVA",  Just ((10,1),DownBack)),("PYTHON", Just ((3,5),Down  )),
              ("PROGRAM",Just ((1,10),Up)),("JULIAN",Just ((8,7), UpBack  )),("SCIENCE",Just ((6,7),UpBack))]


ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F",LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F",LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))
ex3'5 = LamDef [ ( "F",LamAbs 1 (LamApp (LamVar 1) (LamVar 1)) ), ( "G",LamAbs 2 (LamVar 2) ) ] 
               (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 2 (LamVar 2)))

ex3'1Ans = "(\\x1 -> x1) \\x1 -> x1"
ex3'2Ans = "\\x1 -> x1 \\x1 -> x1"
ex3'3Ans = "def F = \\x1 -> x1 in \\x2 -> x2 F"
ex3'4Ans = "def F = \\x1 -> x1 in \\x2 -> F x2"
ex3'5Ans = "def F = \\x1 -> x1 x1 in def G = \\x2 -> x2 in F G"


ex4'1 = "x1 (x2 x3)"
ex4'2 = "x1 x2 F"
ex4'3 = "def F = \\x1-> x1 in \\x2 -> x2 F"
ex4'4 = "def F = \\x1 -> x1 (def G= \\x1 -> x1 in x1) in \\x2 -> x2"
ex4'5 = "def F = \\x1 -> x1 in def F = \\x2 -> x2 x1 in x1"
ex4'6 = "def F = x1 in F"
ex4'7 = "def F = \\x1 -> x1 x1 in def G = \\x2 -> x2 in F G"

ex4'1Ans = Just (LamDef [] (LamApp (LamVar 1) (LamApp (LamVar 2) (LamVar 3))))
ex4'2Ans = Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamMacro"F")))
ex4'3Ans = Just (LamDef  [  ("F",  LamAbs  1  (LamVar  1)  )  ] (LamAbs  2  (LamApp  (LamVar  2)  (LamMacro "F"))))
ex4'4Ans = Nothing
ex4'5Ans = Nothing
ex4'6Ans = Nothing
ex4'7Ans = Just (LamDef [ ( "F",LamAbs 1 (LamApp (LamVar 1) (LamVar 1)) ), ( "G",LamAbs 2 (LamVar 2) ) ] (LamApp (LamMacro "F") (LamMacro "G")))


exId  = LamAbs 1 (LamVar 1)
ex5'1 = LamDef [] (LamApp (LamVar 1) (LamVar 2))
ex5'2 = LamDef [ ("F", exId) ] (LamVar 2)
ex5'3 = LamDef [ ("F", exId) ] (LamMacro "F")
ex5'4 = LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F"))

ex5'1Ans = LamDef [] (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))) 
                                       (LamAbs 4 (LamApp (LamAbs 7 (LamApp (LamVar 7) (LamVar 2)))
                                                         (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5))
                                                                           (LamVar 3)))))))

ex5'2Ans = LamDef [("F",LamAbs 3 (LamApp (LamVar 3)
                                         (LamAbs 1 (LamAbs 4 (LamApp (LamVar 4) 
                                                                     (LamVar 1))))))]
           (LamAbs 5 (LamApp (LamVar 5) (LamVar 2)))

ex5'3Ans = LamDef [("F",LamAbs 2 (LamApp (LamVar 2)
                                         (LamAbs 1 (LamAbs 3 (LamApp (LamVar 3)
                                                                     (LamVar 1))))))]
           (LamMacro "F")

ex5'4Ans = LamDef [("F",LamAbs 2 (LamApp (LamVar 2)
                                         (LamAbs 1 (LamAbs 3 (LamApp (LamVar 3)
                                                                     (LamVar 1))))))]
           (LamAbs 4 (LamApp (LamMacro "F")
                             (LamAbs 5 (LamApp (LamMacro "F")
                                               (LamAbs 6 (LamApp (LamApp (LamVar 5)
                                                                         (LamVar 6))
                                                                 (LamVar 4)))))))


ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))                                                     -- | (\x1 -> x1 x2)
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")                                                                    -- | def F = \x1 -> x1 in F  
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))                                                          -- | (\x1 -> x1) (\x2 -> x2)   
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))                                                                -- | (\x1 -> x1 x1)(\x1 -> x1 x1)  
ex6'4 = LamDef [] (LamApp wExp wExp)
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ]                                         -- | def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
        ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4))) 
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]                                                     -- | def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
        ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]                                         -- | def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1 ) (\x1 -> x1 x1)) ID
        (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 

ex6'1Ans = (Just 0,Just 0,Just 6,Just 6)
ex6'2Ans = (Just 1,Just 1,Just 3,Just 3)
ex6'3Ans = (Just 1,Just 1,Just 8,Just 8)
ex6'4Ans = (Nothing,Nothing,Nothing,Nothing)
ex6'5Ans = (Just 4,Just 4,Just 22,Just 22)
ex6'6Ans = (Just 4,Just 3,Just 21,Just 21)
ex6'7Ans = (Nothing,Just 4,Nothing,Nothing)

ex6'1Inner = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))
ex6'2Inner = Just (LamDef [] (LamApp (LamAbs 1 (LamApp (LamAbs 3 (LamVar 3)) (LamVar 1))) (LamVar 2)))          -- | (\x1 -> (\x3 -> x3) x1) x2
--ex6'3Inner = LamDef [] (LamAbs 1 (LamVar 1))          -- | (\x3 -> x3) x4 (\x1 -> (\x2 -> x2) x1)

ex6'1InnerAns = Nothing
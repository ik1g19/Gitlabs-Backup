{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
  $white+       ; 
  "--".*        ; 
  let           { \p s -> TokenLet p} 
  in            { \p s -> TokenIn p }
  $digit+       { \p s -> TokenInt p (read s) } 
  \=            { \p s -> TokenEq p }
  \+            { \p s -> TokenPlus p }
  \-            { \p s -> TokenMinus p }
  \*            { \p s -> TokenTimes p }
  \/            { \p s -> TokenDiv p }
  \^            { \p s -> TokenExp p }
  \(            { \p s -> TokenLParen p }
  \)            { \p s -> TokenRParen p }
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar p s } 

{ 
-- Each action has type :: AlexPosn -> String -> Token 

-- The token type: 
data Token = 
  TokenLet AlexPosn        | 
  TokenIn  AlexPosn        | 
  TokenInt AlexPosn Int    |
  TokenVar AlexPosn String | 
  TokenEq  AlexPosn        |
  TokenPlus AlexPosn       |
  TokenMinus AlexPosn      |
  TokenTimes AlexPosn      |
  TokenDiv AlexPosn        |
  TokenExp AlexPosn        |
  TokenLParen AlexPosn     |
  TokenRParen AlexPosn      
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenInt  (AlexPn a l c) n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar  (AlexPn a l c) x) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLet (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIn  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTimes (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenExp (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


}
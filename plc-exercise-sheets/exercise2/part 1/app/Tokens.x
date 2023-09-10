{ 
module Tokens where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
  $white+       ; 
  "--".*        ; 
  let           { \s -> TokenLet } 
  in            { \s -> TokenIn }
  $digit+       { \s -> TokenInt (read s) } 
  \=          { \s -> TokenEq }
  \+          { \s -> TokenPlus }
  \-          { \s -> TokenMinus }
  \*          { \s -> TokenTimes }
  \/          { \s -> TokenDiv }
  \(          { \s -> TokenLParen }
  \)          { \s -> TokenRParen }
  \^          { \s -> TokenExp}
  $alpha [$alpha $digit \_ \’]*   { \s -> TokenVar s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenLet         | 
  TokenIn          | 
  TokenInt Int     |
  TokenVar String  | 
  TokenEq          |
  TokenPlus        |
  TokenMinus       |
  TokenTimes       |
  TokenDiv         |
  TokenExp         |
  TokenLParen      |
  TokenRParen       
  deriving (Eq,Show) 

}

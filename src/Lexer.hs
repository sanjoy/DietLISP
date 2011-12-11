module Lexer(tokenize, Token(..)) where

{- A simple Lexer DietLISP. -}

import Data.Char

data Token = LParT | RParT | SymbolT String | IntegerT Integer
             deriving(Eq)

instance Show Token where
  show LParT = "("
  show RParT = ")"
  show (SymbolT s) = s
  show (IntegerT i) = show i

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':rest) = LParT : (tokenize rest)
tokenize (')':rest) = RParT : (tokenize rest)

-- A semicolon starts a comment that continues to the end of the line.
tokenize (';':rest) = tokenize $ dropWhile (/= '\n') rest

-- Check if the current character starts an integer.  Otherwise it starts
-- a symbol
tokenize all@(x:rest) =
  if isSpace x then
    tokenize rest
  else
    if isDigit x then
      let (integer, leftOvers) = parseInteger all
      in (IntegerT $ read integer):(tokenize leftOvers)
    else
      let (symbolString, leftOvers) = parseSymbol all
      in (SymbolT symbolString):(tokenize leftOvers)
  where

  -- Breaks up the string into two parts.  The first part is a parseable
  -- integer and the second contains the remainder of the string.
  parseInteger :: String -> (String, String)
  parseInteger all@(x:xs) =
    if isDigit x then
      let (a, b) = parseInteger xs
      in (x:a, b)
    else
      if isSpace x || x == '(' || x == ')' then
        ("", all)
      else
        error "Bad integer"
  parseInteger [] = ("", "")

  -- Breaks up the string into two parts.  The first part is a valid symbol
  -- and the second part is the remainder of the string.
  parseSymbol :: String -> (String, String)
  parseSymbol all@(x:xs) =
    if isSpace x || x == '(' || x == ')' then
      ("", all)
    else
      let (a, b) = parseSymbol xs
      in (x:a, b)

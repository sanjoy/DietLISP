module Lexer(tokenize, Token(..)) where

{- A simple Lexer DietLISP. -}

import Utils

import Data.Char

data Token = LParT | RParT | SymbolT String | IntegerT Integer
             deriving(Eq)

instance Show Token where
  show LParT = "("
  show RParT = ")"
  show (SymbolT s) = s
  show (IntegerT i) = show i

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize ('(':chars) = do
  rest <- tokenize chars
  return $ LParT:rest

tokenize (')':chars) = do
  rest <- tokenize chars
  return $ RParT:rest

-- A semicolon starts a comment that continues to the end of the line.
tokenize (';':rest) = tokenize $ dropWhile (/= '\n') rest

-- Check if the current character starts an integer.  Otherwise it starts
-- a symbol
tokenize all@(x:rest) =
  if isSpace x then
    tokenize rest
  else
    if isDigit x then do
      (integer, leftOvers) <- parseInteger all
      let token = (IntegerT $ read integer)
      rest <- tokenize leftOvers
      return $ token:rest
    else do
      let (symbolString, leftOvers) = parseSymbol all
      let token = SymbolT symbolString
      rest <- tokenize leftOvers
      return $ token:rest
  where

  -- Breaks up the string into two parts.  The first part is a parseable
  -- integer and the second contains the remainder of the string.
  parseInteger :: String -> Either String (String, String)
  parseInteger all@(x:xs) =
    if isDigit x then do
                   (a, b) <- parseInteger xs
                   return (x:a, b)
    else
      if isSpace x || x == '(' || x == ')' then
        Right ("", all)
      else
        Left $ "Found undesirable character " ++ [x] ++ " when parsing integer"
  parseInteger [] = Right ("", "")

  -- Breaks up the string into two parts.  The first part is a valid symbol
  -- and the second part is the remainder of the string.
  parseSymbol :: String -> (String, String)
  parseSymbol all@(x:xs) =
    if isSpace x || x == '(' || x == ')' then
      ("", all)
    else
      let (a, b) = parseSymbol xs
      in (x:a, b)
  parseSymbol [] = error "parseSymbol [] should never be evaluated."

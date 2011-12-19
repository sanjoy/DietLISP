module DLTokenizer(tokenize, Token(..)) where

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
tokenize all@(x : rest)
  | isSpace x = tokenize rest
  | isDigit x =
      do (integer, leftOvers) <- parseInteger all
         let token = IntegerT $ read integer
         rest <- tokenize leftOvers
         return $ token:rest
  | otherwise =
    do let (symbolString, leftOvers) = parseSymbol all
       rest <- tokenize leftOvers
       return $ (SymbolT symbolString):rest

  where

  -- Breaks up the string into two parts.  The first part is a parseable
  -- integer and the second contains the remainder of the string.
      parseInteger :: String -> Either String (String, String)
      parseInteger all@(x : xs)
        | isDigit x = parseInteger xs >>= (\(a, b)->return (x:a, b))
        | isSpace x || x == '(' || x == ')' = Right ("", all)
        | otherwise =  Left $ "found non-digit " ++ [x] ++ " when parsing integer"

      parseInteger [] = Right ("", "")

      parseSymbol :: String -> (String, String)
      parseSymbol all@(x : xs)
        | isSpace x || x == '(' || x == ')'  = ("", all)
        | otherwise = let (a, b) = parseSymbol xs in (x : a, b)

      parseSymbol [] = error "parseSymbol [] should never be evaluated."

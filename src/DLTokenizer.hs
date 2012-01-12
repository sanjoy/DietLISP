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

tokenize :: String -> MResult String [Token]
tokenize [] = return []
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
    do (symbolString, leftOvers) <- parseSymbol all
       rest <- tokenize leftOvers
       return $ SymbolT symbolString:rest

  where

  -- Breaks up the string into two parts.  The first part is a parseable
  -- integer and the second contains the remainder of the string.
      parseInteger :: String -> MResult String (String, String)
      parseInteger all@(x : xs)
        | isDigit x = parseInteger xs >>= (\(a, b)->return (x:a, b))
        | isSpace x || x `elem` "()" = return ("", all)
        | otherwise =  EResult $ "found non-digit " ++ [x] ++ " when parsing integer"

      parseInteger [] = return ("", "")

      parseSymbol :: String -> MResult String (String, String)
      parseSymbol all@(x : xs)
        | isSpace x || x `elem` "()" = return ("", all)
        | otherwise = parseSymbol xs >>= (\(a, b)->return (x : a, b))

      parseSymbol [] = EResult "unexpected end of input when parsing symbol"

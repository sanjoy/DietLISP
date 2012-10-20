module Tokenizer(tokenize, Token(..)) where

{- A simple Lexer for DietLISP. -}

import Control.Monad(liftM)
import Data.Char(isSpace, isDigit)
import Data.List(find)

import Utils

data Token = LParT | RParT | SymbolT String | IntegerT Integer
           deriving(Eq)

instance Show Token where
  show LParT = "("
  show RParT = ")"
  show (SymbolT s) = s
  show (IntegerT i) = show i

tokenize :: String -> ErrorM [Token]
tokenize [] = return []
tokenize all@(x:rest)
  | x == '(' = liftM (LParT:) $ tokenize rest
  | x == ')' = liftM (RParT:) $ tokenize rest
-- Comments
  | x == ';' = tokenize $ dropWhile (/= '\n') rest
-- Symbols and integers.
  | isSpace x = tokenize $ dropWhile isSpace rest
  | isDigit x =
    let (integer, leftOvers) = span delimited all
    in case find (not . isDigit) integer of
      Just nonDigit ->
        reportError $ "found non-digit " ++ [nonDigit] ++ " when parsing integer"
      Nothing -> liftM ((IntegerT $ read integer):) $ tokenize leftOvers
  | otherwise =
    let (symbolString, leftOvers) = span delimited all
    in liftM (SymbolT symbolString:) $ tokenize leftOvers
  where
    delimited ch = not (isSpace ch || ch `elem` "();")

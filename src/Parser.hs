module Parser(Exp(..), parse) where

{- A shift-reduce parser for DietLISP.  Haskell really shines in these
   sorts of things.  -}

import Lexer

-- DietLISP's AST.
data Exp = ListE [Exp] | SymE String | IntegerE Integer
         | BooleanE Bool deriving(Show, Eq)

-- To be used by the shift-reduce parser.
data StackElement = ValueSE Exp -- A value on the stack
                  | MarkerSE    -- A marker signalling an open parenthesis
                    deriving(Show, Eq)

parse :: [Token] -> [Exp]
parse tokens = stateMachine tokens []

stateMachine :: [Token] -> [StackElement] -> [Exp]

stateMachine [] values = map (\(ValueSE e)->e) values

stateMachine (LParT:rest) stack = stateMachine rest (MarkerSE:stack)

stateMachine (RParT:rest) stack =
  stateMachine rest (ValueSE newExp:merge stack)
    where
      newExp = ListE $ reverse expressions
      expressions = map (\(ValueSE m)->m) $
                    takeWhile (/= MarkerSE) stack
      merge (MarkerSE:rest) = rest
      merge (x:rest) = merge rest

stateMachine (SymbolT s:rest) stack = stateMachine rest $ case s of
  "true"    -> (ValueSE $ BooleanE True):stack
  "false"   -> (ValueSE $ BooleanE False):stack
  otherwise -> (ValueSE $ SymE s):stack

stateMachine (IntegerT i:rest) stack =
  stateMachine rest ((ValueSE $ IntegerE i):stack)

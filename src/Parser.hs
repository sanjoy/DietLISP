module Parser(Exp(..), parse, fullParse) where

{- A shift-reduce parser for DietLISP.  Haskell really shines in these
   sorts of things.  -}

import DLTokenizer
import Utils

-- DietLISP's AST.
data Exp = ListE [Exp] | SymE String | IntegerE Integer
         | BooleanE Bool deriving(Show, Eq)

-- To be used by the shift-reduce parser.
data StackElement = ValueSE Exp -- A value on the stack
                  | MarkerSE    -- A marker signalling an open parenthesis
                    deriving(Show, Eq)

parse :: [Token] -> MResult String [Exp]
parse tokens = stateMachine tokens []

stateMachine :: [Token] -> [StackElement] -> MResult String [Exp]

stateMachine [] values = mapM recover values
  where
    recover (ValueSE e) = return e
    recover _           = EResult "unbalanced parens:  extra '('"

stateMachine (LParT:rest) stack = stateMachine rest (MarkerSE:stack)

stateMachine (RParT:rest) stack = do
  (exps, stackLeft) <- findEnd stack
  let value = ValueSE $ ListE $ reverse exps
  stateMachine rest (value:stackLeft)
    where
      findEnd [] = EResult "unbalanced parens:  extra ')'"
      findEnd (MarkerSE:rest) = return ([], rest)
      findEnd (ValueSE x:xs) = do
        (exps, stackLeft) <- findEnd xs
        return (x:exps, stackLeft)

stateMachine (SymbolT s:rest) stack = stateMachine rest $ case s of
  "true"    -> (ValueSE $ BooleanE True):stack
  "false"   -> (ValueSE $ BooleanE False):stack
  otherwise -> (ValueSE $ SymE s):stack

stateMachine (IntegerT i:rest) stack =
  stateMachine rest ((ValueSE $ IntegerE i):stack)

fullParse :: String -> MResult String [Exp]
fullParse str = tokenize str >>= parse

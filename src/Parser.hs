module Parser(Ast(..), createAst, Ident) where

import Tokenizer
import Utils

import Control.Monad(liftM)

-- DietLISP's AST.
data Exp = ListE [Exp] | SymE String | IntegerE Integer
         | BooleanE Bool deriving(Show, Eq)

castListE _ (ListE es) = return es
castListE string _     = reportError string

castSymE _ (SymE sym) = return sym
castSymE string _     = reportError string

type Ident = String
data Ast = BuiltinA { getBuiltinName :: String }
         | OperativeA { getFormalArgs :: [Ident],
                        getEnvArg :: Ident,
                        getBody :: Ast
                      }
         | ListA { getList :: [Ast] }
         | IdentA { getIdentValue :: Ident }
         | IntA { getIntValue :: Integer }
         | BoolA { getBoolValue :: Bool }
         | GlobalBindA { getBoundIdent :: Ident,
                         getBody :: Ast
                       }
         deriving(Show, Eq)


createAst :: String -> ErrorM [Ast]
createAst input = tokenize input >>= parse >>= mapM createOneAst
  where createOneAst (ListE (SymE "#builtin#":rest)) = do
          builtinId <- extract1 "#builtin#" rest
          builtinIdS <- castSymE "expected builtin name" builtinId
          return $ BuiltinA builtinIdS
        createOneAst (ListE (SymE "operative":rest)) = do
          (args, envArg, body) <- extract3 "operative" rest
          argsList <- castListE "expected list of arguments after operative" args
          argsSymList <- mapM
                         (castSymE "list of arguments may only contain symbols")
                         argsList
          envArgSym <- castSymE "environment argument must be a symbol" envArg
          bodyAst <- createOneAst body
          return $ OperativeA argsSymList envArgSym bodyAst
        createOneAst (ListE (SymE "global-bind":rest)) = do
          (ident, body) <- extract2 "global-bind" rest
          identS <- castSymE "you can only bind to a symbol" ident
          bodyAst <- createOneAst body
          return $ GlobalBindA identS bodyAst
        createOneAst (ListE list) = do
          astList <- mapM createOneAst list
          return $ ListA astList
        createOneAst (SymE sym) = return $ IdentA sym
        createOneAst (IntegerE int) = return $ IntA int
        createOneAst (BooleanE bool) = return $ BoolA bool

-- Used by the shift-reduce parser.
data StackElement = ValueSE Exp -- A value on the stack
                  | MarkerSE    -- A marker signalling an open parenthesis
                    deriving(Show, Eq)

parse :: [Token] -> ErrorM [Exp]
parse tokens = liftM reverse $ stateMachine tokens []

stateMachine :: [Token] -> [StackElement] -> ErrorM [Exp]

stateMachine [] values = mapM recover values
  where
    recover (ValueSE e) = return e
    recover _           = reportError "unbalanced parens:  extra '('"

stateMachine (LParT:rest) stack = stateMachine rest (MarkerSE:stack)

stateMachine (RParT:rest) stack = do
  (exps, stackLeft) <- findEnd stack
  let value = ValueSE $ ListE $ reverse exps
  stateMachine rest (value:stackLeft)
    where
      findEnd [] = reportError "unbalanced parens:  extra ')'"
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

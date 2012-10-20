module Parser(Ast(..), RawAST(..), cookAST, uncookAST, Ident,
              fullParse) where

-- The parser first parses the token stream into a RawAST and then
-- "cooks" the RawAST into an AST proper.  "cooking" is reversible.
-- Apart from keeping the design a little bit cleaner, this dual
-- representation helps us manipulate ASTs as lists.

import Tokenizer
import Utils

import Control.Monad(liftM)

data RawAST = ListE [RawAST] | SymE String | IntegerE Integer
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

fullParse :: String -> ErrorM [Ast]
fullParse input = tokenize input >>= parseRaw >>= mapM cookAST

builtinSym = SymE "#builtin#"
operativeSym = SymE "operative"
globalBindSym = SymE "global-bind"
unsym (SymE string) = string

cookAST :: RawAST -> ErrorM Ast
cookAST (ListE list@(car:cdr))
  | car == builtinSym = do
    builtinId <- extract1 (unsym builtinSym) cdr
    builtinIdS <- castSymE "expected builtin name" builtinId
    return $ BuiltinA builtinIdS
  | car == operativeSym = do
    (args, envArg, body) <- extract3 (unsym operativeSym) cdr
    argsList <- castListE "expected list of arguments after operative" args
    argsSymList <- mapM
                   (castSymE "list of arguments may only contain symbols")
                   argsList
    envArgSym <- castSymE "environment argument must be a symbol" envArg
    bodyAst <- cookAST body
    return $ OperativeA argsSymList envArgSym bodyAst
  | car == globalBindSym = do
    (ident, body) <- extract2 (unsym globalBindSym) cdr
    identS <- castSymE "you can only bind to a symbol" ident
    bodyAst <- cookAST body
    return $ GlobalBindA identS bodyAst
  | otherwise = do
    astList <- mapM cookAST list
    return $ ListA astList
cookAST (SymE sym) = return $ IdentA sym
cookAST (IntegerE int) = return $ IntA int
cookAST (BooleanE bool) = return $ BoolA bool

uncookAST :: Ast -> RawAST
uncookAST (BuiltinA bName) = ListE [builtinSym, SymE bName]
uncookAST (OperativeA formalArgs envArg ast) =
  ListE [operativeSym, ListE $ map SymE formalArgs, SymE envArg, uncookAST ast]
uncookAST (ListA asts) = ListE $ map uncookAST asts
uncookAST (IdentA string) = SymE string
uncookAST (IntA integer) = IntegerE integer
uncookAST (BoolA boolean) = BooleanE boolean
uncookAST (GlobalBindA boundIdent body) =
  ListE [globalBindSym, SymE boundIdent, uncookAST body]

data StackElement = ValueSE RawAST -- A value on the stack
                  | MarkerSE    -- A marker signalling an open parenthesis
                  deriving(Eq)

parseRaw :: [Token] -> ErrorM [RawAST]
parseRaw tokens = liftM reverse $ stateMachine tokens []

stateMachine :: [Token] -> [StackElement] -> ErrorM [RawAST]

stateMachine [] values = mapM recover values
  where recover (ValueSE e) = return e
        recover _           = reportError "unbalanced parens:  extra '('"
stateMachine (LParT:rest) stack = stateMachine rest (MarkerSE:stack)
stateMachine (RParT:rest) stack = do
  let (exps, stackLeft) = span (MarkerSE /=) stack
  if null stackLeft then reportError "unbalanced parens:  extra ')'"
    else stateMachine rest
         ((ValueSE $ ListE $ reverse $ map unwrap exps):tail stackLeft)
    where unwrap (ValueSE v) = v

stateMachine (SymbolT s:rest) stack = stateMachine rest $ case s of
  "true"    -> (ValueSE $ BooleanE True):stack
  "false"   -> (ValueSE $ BooleanE False):stack
  otherwise -> (ValueSE $ SymE s):stack

stateMachine (IntegerT i:rest) stack =
  stateMachine rest ((ValueSE $ IntegerE i):stack)

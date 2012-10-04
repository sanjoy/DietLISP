module Builtins(Builtin, lookupBuiltin, getOperative) where

import Control.Arrow(second)
import Data.List(find)
import Data.Maybe(fromMaybe)
import qualified Data.Map

import Domain
import Parser

data Builtin = B { getOperative :: (Env -> Ast -> Domain) -> Env -> [Ast] -> Domain,
                   getName :: String }

instance Show Builtin where
  show = getName

currentBuiltins :: Data.Map.Map String Builtin
currentBuiltins = Data.Map.fromList $
                  simplePredicates ++ listOperatives ++ arithmeticOperatives ++
                  [ifOperative] ++ [letOperative] ++ evalOperatives ++
                  envOperatives

lookupBuiltin s = Data.Map.lookup s currentBuiltins

liftUnary predicate (BottomD string) = BottomD string
liftUnary predicate domainValue = predicate domainValue

createBuiltinTuple (name, operative) = ("#" ++ name ++ "#",
                                        B operative ("#" ++ name ++ "#"))

invalidArgsErr fn = BottomD $ "invalid arguments to " ++ fn

-- Wrapper for strict operatives
runEvaluated fn eval env asts =
  let evaluatedArgs = map (eval env) asts
  in fromMaybe (fn evaluatedArgs) (find isBottom evaluatedArgs)
    where isBottom (BottomD _) = True
          isBottom _ = False

-- Simple predicates
predicateTransform (name, operative) = (name, runEvaluated lifted)
  where lifted [value] = BooleanD $ operative value
        lifted _ = invalidArgsErr name

integerp (IntegerD _) = True
integerp _ = False

booleanp (BooleanD _) = True
booleanp _ = False

listp (BuiltinD (ListBD _)) = True
listp _ = False

simplePredicates = map (createBuiltinTuple . predicateTransform)
                   [("integerp", integerp), ("booleanp", booleanp),
                    ("listp", listp)]

-- List operatives
listCons [head, BuiltinD (ListBD tail)] = BuiltinD (ListBD (head:tail))
listCons _ = invalidArgsErr "cons"

listHead [head, BuiltinD (ListBD (x:xs))] = x
listHead [head, BuiltinD (ListBD [])] = BottomD "head on empty list"
listHead _ = invalidArgsErr "head"

listTail [head, BuiltinD (ListBD (x:xs))] = BuiltinD $ ListBD xs
listTail [head, BuiltinD (ListBD [])] = BottomD "tail on empty list"
listTail _ = invalidArgsErr "tail"

listOperatives = map (createBuiltinTuple . second runEvaluated)
                 [("cons", listCons), ("head", listHead), ("tail", listTail)]

-- Integer arithmetic
hostArithmetic (name, hostOp) = (name, runEvaluated domainOp)
  where domainOp [IntegerD i1, IntegerD i2] = IntegerD $ hostOp i1 i2
        domainOp _ = invalidArgsErr name

arithmeticOperatives = map (createBuiltinTuple . hostArithmetic)
                       [("plus", (+)), ("minus", (-)), ("mult", (*)),
                        ("div", div), ("rem", rem)]

-- boolean operatives should be implemented in DietLISP itself using
-- just this primitive.
ifOperative = createBuiltinTuple ("if", builtinIf)
  where builtinIf eval env [cond, true, false] = case eval env cond of
          BooleanD conditionValue ->
            if conditionValue then eval env true else eval env false
          BottomD reason -> BottomD reason
          _ -> invalidArgsErr "if"

-- Recursive let
letOperative = createBuiltinTuple ("let", builtinLet)
  where builtinLet eval env [ListA [IdentA ident, value], body] =
          let recursiveEnv = addBinding env (ident, eval recursiveEnv value)
              evaluatedValue = eval recursiveEnv value
          in case evaluatedValue of
            BottomD reason -> BottomD reason
            value -> eval (addBinding env (ident, value)) body
        builtinLet _ _ _ = invalidArgsErr "let"

-- The eval operatives
evalOperatives = map createBuiltinTuple [("eval", builtinEval),
                                         ("eval*", builtinEvalS)]
  where builtinEval eval env [envAst, ast] = case eval env envAst of
          EnvD effectiveEnv -> eval effectiveEnv ast
          x -> BottomD $ "environment not correct: " ++ show x
        builtinEval _ _ _ = invalidArgsErr "eval"
        builtinEvalS eval env [envAst, ast] = case eval env ast of
          BottomD reason -> BottomD reason
          AstD ast -> builtinEval eval env [envAst, ast]
          others -> BottomD $ show others ++ " is not an ast"

-- Operatives to play around with the environment
envOperatives = map createBuiltinTuple [("current-env", currentEnv)]
  where currentEnv _ env [] = EnvD env
        currentEnv _ _ _ = invalidArgsErr "current-env"

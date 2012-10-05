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
                  envOperatives ++ [equalOperative]

lookupBuiltin s = Data.Map.lookup s currentBuiltins

liftUnary predicate (BottomD string) = BottomD string
liftUnary predicate domainValue = predicate domainValue

createBuiltinTuple (name, operative) = ("#" ++ name ++ "#",
                                        B operative ("#" ++ name ++ "#"))

invalidArgsErr fn args = BottomD $ "invalid arguments to " ++ fn ++ ": " ++ show args

-- Wrapper for strict operatives
runEvaluated fn eval env asts =
  let evaluatedArgs = map (eval env) asts
  in fromMaybe (fn evaluatedArgs) (find isBottom evaluatedArgs)
    where isBottom (BottomD _) = True
          isBottom _ = False

-- Simple predicates
predicateTransform (name, operative) = (name, runEvaluated lifted)
  where lifted [value] = BooleanD $ operative value
        lifted args = invalidArgsErr name args

integerp (IntegerD _) = True
integerp _ = False

booleanp (BooleanD _) = True
booleanp _ = False

listp (BuiltinD (ListBD _)) = True
listp _ = False

astp (AstD _) = True
astp _ = False

envp (EnvD _) = True
envp _ = False

simplePredicates = map (createBuiltinTuple . predicateTransform)
                   [("integerp", integerp), ("booleanp", booleanp),
                    ("listp", listp), ("astp", astp), ("envp", envp)]

-- List operatives
listCons [head, BuiltinD (ListBD tail)] = BuiltinD (ListBD (head:tail))
listCons args = invalidArgsErr "cons" args

listHead [BuiltinD (ListBD (x:xs))] = x
listHead [BuiltinD (ListBD [])] = BottomD "head on empty list"
listHead args = invalidArgsErr "head" args

listTail [BuiltinD (ListBD (x:xs))] = BuiltinD $ ListBD xs
listTail [BuiltinD (ListBD [])] = BottomD "tail on empty list"
listTail args = invalidArgsErr "tail" args

listNilP [BuiltinD (ListBD [])] = BooleanD True
listNilP [BuiltinD (ListBD _)] = BooleanD False
listNilP args = invalidArgsErr "nilp" args

listNilValue [] = BuiltinD (ListBD [])
listNilValue args = invalidArgsErr "nil" args

listOperatives = map (createBuiltinTuple . second runEvaluated)
                 [("cons", listCons), ("head", listHead), ("tail", listTail),
                  ("nilp", listNilP), ("nil", listNilValue)]

-- Integer arithmetic
hostArithmetic (name, hostOp) = (name, runEvaluated domainOp)
  where domainOp [IntegerD i1, IntegerD i2] = IntegerD $ hostOp i1 i2
        domainOp args = invalidArgsErr name args

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
          args -> invalidArgsErr "if" args

-- Recursive let
letOperative = createBuiltinTuple ("let", builtinLet)
  where builtinLet eval env [ListA [IdentA ident, value], body] =
          let recursiveEnv = addBinding env (ident, eval recursiveEnv value)
              evaluatedValue = eval recursiveEnv value
          in case evaluatedValue of
            BottomD reason -> BottomD reason
            value -> eval (addBinding env (ident, value)) body
        builtinLet _ _ args = invalidArgsErr "let" args

-- The eval operatives
evalOperatives = map createBuiltinTuple [("eval", builtinEval),
                                         ("eval*", builtinEvalS)]
  where builtinEval eval env [envAst, ast] = case eval env envAst of
          EnvD effectiveEnv -> eval effectiveEnv ast
          x -> BottomD $ "environment not correct: " ++ show x
        builtinEval _ _ args = invalidArgsErr "eval" args
        builtinEvalS eval env [envAst, ast] = case eval env ast of
          BottomD reason -> BottomD reason
          AstD ast -> builtinEval eval env [envAst, ast]
          others -> BottomD $ show others ++ " is not an ast"

-- Operatives to play around with the environment
envOperatives = map createBuiltinTuple [("current-env", currentEnv),
                                        ("add-binding", addBindingBuiltin)]
  where currentEnv _ env [] = EnvD env
        currentEnv _ _ args = invalidArgsErr "current-env" args
        addBindingBuiltin eval env [rootEnvAst, IdentA ident, value] =
          case eval env rootEnvAst of
            EnvD rootEnv -> EnvD (addBinding rootEnv (ident, eval env value))
            result -> BottomD $ "not valid environment " ++ show result
        addBindingBuiltin _ _ args = invalidArgsErr "binding-add" args

-- Equality
equalOperative = createBuiltinTuple ("equal", runEvaluated equality)
  where equality [OperativeD _, OperativeD _] = BooleanD False
        equality [IntegerD i1, IntegerD i2] = BooleanD $ i1 == i2
        equality [BooleanD b1, BooleanD b2] = BooleanD $ b1 == b2
        equality [EnvD _, EnvD _] = BooleanD False
        equality [BuiltinD (ListBD l1), BuiltinD (ListBD l2)] =
          if length l1 == length l2 then
            BooleanD $ all (\(a, b) -> unwrap $ equality [a, b]) $ zip l1 l2
          else
            BooleanD False
        equality [AstD ast1, AstD ast2] = BooleanD $ ast1 == ast2
        equality [_, _] = BooleanD False
        equality args = invalidArgsErr "equal" args
        unwrap (BooleanD v) = v

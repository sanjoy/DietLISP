module Semantics(Domain, Env, evaluate, evalTopLevel) where

import Data.Maybe(fromMaybe)

import Builtins
import Domain
import Parser
import Utils

evaluate :: Env -> Ast -> Domain
evaluate env (BuiltinA builtin) = case lookupBuiltin builtin of
  Nothing -> BottomD $ "no builtin named " ++ builtin
  Just builtin -> OperativeD $ getOperative builtin evaluate
evaluate env (OperativeA formalArgs envArg body) = OperativeD executeOperative
  where executeOperative actualEnv actualArgs =
          fromMaybe (BottomD "invalid number of arguments")
          (maybeApp actualArgs actualEnv)
        maybeApp actualArgs actualEnv = do
          bindArguments <- correctZip formalArgs $ map AstD actualArgs
          let newEnv = addBindings env ((envArg, EnvD actualEnv):bindArguments)
          return $ evaluate newEnv body
evaluate env (ListA (operator:args)) = case evaluate env operator of
  (OperativeD operativeFn) -> operativeFn env args
  (BottomD reason) -> BottomD reason
  _ -> BottomD "only operatives may be applied"
evaluate env (ListA []) = BottomD "the empty list can't be evaluated"
evaluate env (GlobalBindA _ _) =
  BottomD "global-bind is only allowed in the top level"
evaluate env (IdentA ident) = resolveBinding env ident
evaluate _ (IntA i) = IntegerD i
evaluate _ (BoolA b) = BooleanD b

evalTopLevel :: Env -> Ast -> (Env, Domain)
evalTopLevel env (GlobalBindA ident ast) =
  let recursiveEnv = addBinding env (ident, evaluate recursiveEnv ast)
      evaluatedBody = evaluate recursiveEnv ast
      newEnv = addBinding env (ident, evaluatedBody)
  in (newEnv, evaluatedBody)
evalTopLevel env ast = (env, evaluate env ast)

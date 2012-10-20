module Domain(Env, addBinding, addBindings, resolveBinding, emptyEnv,
              envDomain, Domain(..), BuiltinDomain(..)) where

import Data.List(intercalate)
import qualified Data.Map

import Parser

newtype Env = Env (Data.Map.Map Ident (Domain, Env))

addBinding :: Env -> (Ident, Domain) -> Env
addBinding all@(Env e) (ident, value) = Env (Data.Map.insert ident (value, all) e)

addBindings :: Env -> [(Ident, Domain)] -> Env
addBindings = foldl addBinding

envDomain (Env env) = map fst $ Data.Map.toList env

resolveBinding :: Env -> Ident -> Domain
resolveBinding (Env env) ident = case Data.Map.lookup ident env of
  Nothing -> BottomD $ "identifier " ++ ident ++ " couldn't be resolved.  " ++
             "I can see the symbols " ++ show (envDomain (Env env))
  Just (d, _) -> d

emptyEnv = Env Data.Map.empty

data BuiltinDomain = ListBD [Domain] | SymBD String

data Domain = OperativeD (Env -> [Ast] -> Domain)
            | IntegerD Integer
            | BooleanD Bool
            | EnvD Env
            | BuiltinD BuiltinDomain
            | AstD Ast
            | BottomD String

instance Show Domain where
  show (OperativeD _) = "#operative#"
  show (IntegerD i) = show i
  show (BooleanD b) = if b then "true" else "false"
  show (EnvD env) = show env
  show (BuiltinD builtin) = show builtin
  show (AstD ast) = "#ast < " ++ show ast ++ " >"
  show (BottomD reason) = "bottom < " ++ reason ++ " >"

instance Show BuiltinDomain where
  show (ListBD domains) = "[" ++ intercalate "," (map show domains) ++ "]"
  show (SymBD sym) = "'" ++ sym

instance Show Env where
  show (Env mapping) = show mapping

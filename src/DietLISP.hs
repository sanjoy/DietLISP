module Main(main) where

import Control.Monad(liftM)
import Data.List(intercalate)
import System.Environment(getArgs)
import System.IO(putStrLn, hGetContents, hClose, openFile, IOMode(ReadMode))

import Domain
import Parser
import Semantics
import Utils

multiEval env [] = []
multiEval prevEnv (ast:asts) =
  let (newEnv, value) = evalTopLevel prevEnv ast
  in (value:multiEval newEnv asts)

multiEvalString string = liftM (multiEval emptyEnv) $ fullParse string

runFile fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  case liftM (map show) $ multiEvalString contents of
    Result a -> mapM_ putStrLn a
    Error str -> putStrLn $ "error: " ++ str
  hClose handle

usage = do
  version
  putStrLn "Usage: dlisp [--repl] [file name]"

version = putStrLn "DietLISP 0.2 (c) Sanjoy Das"

main = do
  args <- getArgs
  case args of
    ["--version"]         -> version
    ["--usage"]           -> usage
    [fileName]            -> runFile fileName
    otherwise             -> usage

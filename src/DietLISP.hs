module Main(main) where

import Control.Exception
import Parser
import REPL
import Semantics

import Data.List(intercalate)
import System(getArgs)
import System.IO

runFile fileName debug = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let results = eval contents
  case results of
    Left string -> putStrLn $ "error:  " ++ string
    Right results -> putStrLn $ showResults results
  hClose handle
    where
      showResults :: (Show a) => [a] -> String
      showResults s = intercalate "\n" $ map show s

usage = do
  version
  putStrLn "Usage: dlisp [--repl] [file name]"

version = putStrLn "DietLISP 0.1 (c) Sanjoy Das"

main = do
  args <- getArgs
  case args of
    []                    -> runREPL
    ["--repl"]            -> runREPL
    ["--version"]         -> version
    ["--usage"]           -> usage
    [fileName]            -> runFile fileName False
    ["--debug", fileName] -> runFile fileName True
    otherwise             -> usage

module Main(main) where

import Control.Exception
import Parser
import REPL
import Semantics
import Utils

import Data.List(intercalate)
import System(getArgs)
import System.IO

wrapEval :: String -> String -> String
wrapEval expString inputString =
  let inputs = map read $ words inputString
      results = eval expString inputs
  in case results of
    EResult string  -> ("error:  " ++ string)
    CResult results -> (showResults results)
    where
      showResults s = intercalate "\n" $ map show s

runFile fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  interact (wrapEval contents)
  hClose handle

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
    [fileName]            -> runFile fileName
    otherwise             -> usage

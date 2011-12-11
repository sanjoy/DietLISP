module Main(main) where

import Control.Exception
import Lexer
import Parser
import Semantics

import System(getArgs)
import System.IO

runFile fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  Control.Exception.catch (putStrLn $ show $ eval contents)
        ((\e->putStrLn "⊥")::Control.Exception.PatternMatchFail->IO())
  hClose handle

usage = do
  version
  putStrLn "Usage: dlisp [file name]"

version = do
  putStrLn "DietLISP 0.1 (c) Sanjoy Das"

main = do
  args <- getArgs
  case args of
    ["--version"] -> version
    ["--usage"]   -> usage
    [fileName]    -> runFile fileName
    otherwise     -> usage

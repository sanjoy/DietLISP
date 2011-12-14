module Main(main) where

import Control.Exception
import Lexer
import Parser
import Semantics

import Data.List(intercalate)
import System(getArgs)
import System.IO

runFile fileName debug = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  if not debug then
      Control.Exception.catch (putStrLn $ showResults $ eval contents)
         ((\e->putStrLn "⊥")::Control.Exception.PatternMatchFail->IO())
      else
        putStrLn $ showResults $ eval contents
  hClose handle
    where
      showResults :: (Show a) => [a] -> String
      showResults s = intercalate "\n" $ map show s

usage = do
  version
  putStrLn "Usage: dlisp [file name]"

version = do
  putStrLn "DietLISP 0.1 (c) Sanjoy Das"

main = do
  args <- getArgs
  case args of
    ["--version"]         -> version
    ["--usage"]           -> usage
    [fileName]            -> runFile fileName False
    ["--debug", fileName] -> runFile fileName True
    otherwise             -> usage

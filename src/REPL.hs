module REPL where

import System.Console.Readline

import Parser(fullParse)
import Semantics

data REPLState = REPLState String Bindings deriving(Show, Eq)

freshState = REPLState "" builtins

rMapContext :: (a -> b -> Either String (a, c)) -> a -> [b] -> ([Either String c], a)
rMapContext _ bindings [] = ([], bindings)
rMapContext f oldSeed (current:bs) =
    case f oldSeed current of
      Left errorStr           -> let (p, q) = rMapContext f oldSeed bs
                                 in (Left errorStr:p, q)
      Right (newSeed, result) -> let (p, q) = rMapContext f newSeed bs
                                 in (Right result:p, q)

repl :: REPLState -> String -> (REPLState, [Either String Result])
repl (REPLState oldStr oldB) string =
    let (toEval, leftOvers) = bootstrap (oldStr ++ string) []
    in if toEval /= "" then
           case fullParse toEval of
             Left error -> (REPLState leftOvers oldB, [Left $ "could not parse `" ++ toEval ++ "`" ++ error])
             Right exps -> let (results, bindings) = rMapContext evalTopLevel oldB exps
                           in (REPLState leftOvers bindings, results)
       else
           (REPLState leftOvers oldB, [])
    where
      bootstrap ('(':rest) buffer = extract rest ('(':buffer)  1
      bootstrap (')':rest) buffer = extract rest (')':buffer) (-1)
      bootstrap (x:rest)   buffer = bootstrap rest (x:buffer)
      bootstrap []         buffer = extract [] buffer 0

      extract rest buffer 0 = (reverse buffer, rest)
      extract ('(':rest) buffer n = extract rest ('(':buffer) (n + 1)
      extract (')':rest) buffer n = extract rest (')':buffer) (n - 1)
      extract (x:rest)   buffer n = extract rest (x:buffer)   n
      extract []         buffer n = ([], reverse buffer)

runREPL = delegate freshState
    where
      printResult (Left error)   = putStrLn $ "error:  " ++ error
      printResult (Right result) = print result
      delegate state = do
        thisLine <- readline "dlisp % "
        case thisLine of
          Nothing   -> return ()
          Just text -> do addHistory text
                          let (newState, results) = repl state (" " ++ text ++ " ")
                          mapM_ printResult results
                          delegate newState

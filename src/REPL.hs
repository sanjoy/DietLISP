module REPL(runREPL) where

import System.Console.Readline

import Parser(fullParse, Exp (WorldE))
import Semantics
import Utils

data REPLState = REPLState String Bindings deriving(Show, Eq)

freshState = REPLState "" builtins

rMapContext :: (a -> b -> MResult String (a, c)) -> a -> [b] -> ([MResult String c], a)
rMapContext _ bindings [] = ([], bindings)
rMapContext f oldSeed (current:bs) =
    case f oldSeed current of
      EResult errorStr          -> let (p, q) = rMapContext f oldSeed bs
                                   in (EResult errorStr:p, q)
      CResult (newSeed, result) -> let (p, q) = rMapContext f newSeed bs
                                   in (return result:p, q)

repl :: REPLState -> String -> (REPLState, [MResult String Result])
repl (REPLState oldStr oldB) string =
    let (toEval, leftOvers) = bootstrap (oldStr ++ string) []
    in if toEval /= "" then
           case fullParse toEval of
             EResult error ->
               (REPLState leftOvers oldB,
                [EResult $ "could not parse `" ++ toEval ++ "`" ++ error])
             CResult exps ->
               let (results, bindings) =
                     rMapContext(evalTopLevel $ WorldE [] []) oldB exps
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
      printResult (EResult error)  = putStrLn $ "error:  " ++ error
      printResult (CResult result) = print result
      delegate state = do
        thisLine <- readline "dlisp % "
        case thisLine of
          Nothing   -> return ()
          Just text -> do addHistory text
                          let (newState, results) = repl state (" " ++ text ++ " ")
                          mapM_ printResult results
                          delegate newState

module Semantics(eval, evalTopLevel, Bindings, Result, builtins) where

{- Denotational semantics-like reduction rules.  -}

import DLTokenizer
import Origin
import Parser
import Utils

import Control.Monad(foldM, liftM)
import Data.Map(Map, empty, fromList, toList, insert, lookup)
import Data.Function(fix)

-- Pretty way to prevent clashes.
emptyM  = empty
insertM :: (Ord k) => k -> a -> Map k a -> Map k a
insertM = Data.Map.insert
lookupM :: (Ord k) => k -> Map k a -> Maybe a
lookupM = Data.Map.lookup

type Bindings = Map String Result

-- An expression evaluates to this type.
data Result = IntegerR Integer
            | BooleanR Bool
            | ListR [Result]
            | SymbolR Exp
            | LambdaR Bindings {- Closure -} [String] {- Args -}  Exp {- Body -}
            | MacroR Bindings Exp
            | UndefinedR String {- Error -}
            | ThunkR Bindings Exp
              deriving(Eq)

-- Prettier display
instance Show Result where
  show (IntegerR i)                = show i
  show (BooleanR value)            = if value then "true" else "false"
  show (ListR l)                   = "(" ++ unwords (map show l) ++ ")"
  show (SymbolR (SymE s))          = '\'':s
  show (SymbolR (IntegerE i))      = show i
  show (SymbolR (BooleanE value))  = if value then "true" else "false"
  show (SymbolR (ListE _))         = error "There should be no SymbolR (ListE) instances!"
  show (LambdaR _ _ _)             = "## Function Object ##"
  show (MacroR _ _)                = "## Macro Object ##"
  show (ThunkR _ _)                = "## Thunk Object ##"
  show (UndefinedR s)              = "bottom (" ++ s ++ ")"

isUndef (UndefinedR _)  = True
isUndef _               = False

smallPredicate name bindings operands predicate = do
  operand <- extract1 name operands
  result <- evaluate bindings operand
  if isUndef result then
    return result
    else
      return $ predicate result

castListE _ (ListE es) = return es
castListE string _     = EResult string

castSymE _ (SymE sym) = return sym
castSymE string _     = EResult string

-- Fold OPERANDS (assumed integers) using OPERATION seeding with BEGIN in
-- context BINDINGS.  Result in UndefinedR in case of operands of
-- incorrect type, IntegerR otherwise.
nAryOp bindings operands begin operation = do
  integers <- mapM (evaluate bindings) operands
  return $ foldl evalOp (IntegerR begin) integers
    where
      evalOp (IntegerR i) (IntegerR j) = IntegerR $ operation i j
      {-  The ordering of the two following clauses is important.  This ensures
          that expressions like (if (+ a b) 0 1) report an error about symbol `a`
          and not `b` (assuming neither of them are defined).  -}
      evalOp (UndefinedR x) _          = UndefinedR x
      evalOp _ (UndefinedR x)          = UndefinedR x
      evalOp _ _                       =
          UndefinedR "attempted to apply integer operation to non-integers"

binaryOp bindings l r evalFn = do
  eLeft <- evaluate bindings l
  eRight <- evaluate bindings r
  case (eLeft, eRight) of
    (IntegerR i, IntegerR j) -> return $ evalFn i j
    (UndefinedR x, _)        -> return $ UndefinedR x
    (_, UndefinedR x)        -> return $ UndefinedR x
    (_, _) -> return $ UndefinedR "integer operation applied to non-integers"

intBinOp  bindings op (l, r) = binaryOp bindings l r (\i j -> IntegerR $ op i j)
boolBinOp bindings op (l, r) = binaryOp bindings l r (\i j -> BooleanR $ op i j)

argsError name n = EResult $ "`" ++ name ++ "` expects only " ++ n ++ " argument" ++
                   (if name == "one" then "" else "s")

extract1 :: String -> [Exp] -> MResult String Exp
extract1 _ [x]  = return x
extract1 name _ = argsError name "one"

extract2 :: String -> [Exp] ->  MResult String (Exp, Exp)
extract2 _ [x, y] = return (x, y)
extract2 name _   = argsError name "two"

extract3 :: String -> [Exp] -> MResult String (Exp, Exp, Exp)
extract3 _ [x, y, z] = return (x, y, z)
extract3 name _      = argsError name "three"

evaluate :: Bindings -> Exp -> MResult String Result

evaluate _ (IntegerE i) = return $ IntegerR i
evaluate _ (BooleanE b) = return $ BooleanR b
evaluate _ (SymE "null") = return $ ListR []

evaluate bindings (SymE s) =
  case lookupM s bindings of
    Just (ThunkR oldB exp) -> evaluate oldB exp
    Just result            -> return result
    Nothing                -> return $ UndefinedR $ resolutionError s bindings
      where
        resolutionError symbol bindings =
          let list = show $ map fst $ toList bindings
          in "can't resolve symbol `" ++ symbol ++ "`"

-- Basic arithmetic.
evaluate bindings (ListE (SymE "+":addends))  = nAryOp bindings addends 0 (+)
evaluate bindings (ListE (SymE "*":addends))  = nAryOp bindings addends 1 (*)
evaluate bindings (ListE (SymE "-":ops))      = extract2 "-" ops >>= intBinOp bindings (-)
evaluate bindings (ListE (SymE "/":ops))      = extract2 "/" ops >>= intBinOp bindings div
evaluate bindings (ListE (SymE "%":ops))      = extract2 "%" ops >>= intBinOp bindings mod

-- Relational arithmetic
evaluate bindings (ListE (SymE ">":ops))   = extract2 ">"  ops >>= boolBinOp bindings (>)
evaluate bindings (ListE (SymE ">=":ops))  = extract2 ">=" ops >>= boolBinOp bindings (>=)
evaluate bindings (ListE (SymE "<":ops))   = extract2 "<"  ops >>= boolBinOp bindings (<)
evaluate bindings (ListE (SymE "<=":ops))  = extract2 "<=" ops >>= boolBinOp bindings (<=)

-- (if condition true-value false-value)
-- :: typeof (true-value) == typeof (false-value)
evaluate bindings (ListE (SymE "if":operands)) = do
  (condition, t, e) <- extract3 "if" operands
  eCondition <- evaluate bindings condition
  if isUndef eCondition
    then return eCondition
    else evaluateIf eCondition t e
    where
      evaluateIf (BooleanR True) a _   = evaluate bindings a
      evaluateIf (BooleanR False)  _ b = evaluate bindings b
      evaluateIf _  _  _ = return $ UndefinedR "condition non-boolean in `if`"

-- (cons head tail)
-- :: ([x], x) -> [x]
evaluate bindings (ListE (SymE "cons":operands)) = do
  (head, tail) <- extract2 "cons" operands
  eHead <- evaluate bindings head
  if isUndef eHead
    then return eHead
    else do
      eTail <- evaluate bindings tail
      if isUndef eTail
        then return eTail
        else return $ evaluateCons eHead eTail
    where
      evaluateCons :: Result -> Result -> Result
      evaluateCons a (ListR b) = ListR (a:b)
      evaluateCons _ _         = UndefinedR "you can cons only to a list"

-- (head list)
-- :: [x] -> x
evaluate bindings (ListE (SymE "head":operands)) = do
  list <- extract1 "head" operands
  eList <- evaluate bindings list
  if isUndef eList
    then return eList
    else return $ evaluateHead eList
    where
      evaluateHead (ListR (x:_)) = x
      evaluateHead (ListR [])    = UndefinedR "can't take head of empty list"
      evaluateHead _             = UndefinedR "can't take head of non-list"

-- (tail list)
-- :: [x] -> [x]
evaluate bindings (ListE (SymE "tail":operands)) = do
  list <- extract1 "head" operands
  eList <- evaluate bindings list
  if isUndef eList
    then return eList
    else return $ evaluateTail eList
    where
      evaluateTail (ListR (_:xs)) = ListR xs
      evaluateTail (ListR [])     = UndefinedR "can't take tail of empty list"
      evaluateTail _              = UndefinedR "can't take tail of non-list"

-- (list x0 x1 x2 ...)
evaluate bindings (ListE (SymE "list":rest)) = do
  results <- mapM (evaluate bindings) rest
  return $ ListR results

-- (== a b)
-- :: x -> x -> Bool
evaluate bindings (ListE (SymE "==":operands)) = do
  (left, right) <- extract2 "==" operands
  eLeft <- evaluate bindings left
  if isUndef eLeft
    then return eLeft
    else do
      eRight <- evaluate bindings right
      if isUndef eRight
        then return eRight
        else return $ BooleanR $ eLeft == eRight

-- (integerp a)
evaluate bindings (ListE (SymE "integerp":operands)) =
  smallPredicate "integerp" bindings operands integerp
    where
      integerp (IntegerR _) = BooleanR True
      integerp _            = BooleanR False

-- (listp a)
evaluate bindings (ListE (SymE "listp":operands)) =
  smallPredicate "listp" bindings operands listp
    where
      listp (ListR _) = BooleanR True
      listp _         = BooleanR False

-- (boolp a)
evaluate bindings (ListE (SymE "boolp":operands)) =
  smallPredicate "boolp" bindings operands boolp
    where
      boolp (BooleanR _) = BooleanR True
      boolp _            = BooleanR False

-- (symbolp a)
evaluate bindings (ListE (SymE "symbolp":operands)) =
  smallPredicate "symbolp" bindings operands symbolp
    where
      symbolp (SymbolR _) = BooleanR True
      symbolp _           = BooleanR False

-- (let ((var0 value0) (var1 value1) ...) expression)
-- :: typeof (expression)
evaluate bindings (ListE (SymE "let":rest)) =
  do
    (vars, expression) <- extract2 "let" rest
    variables <- castListE "syntax error in `let` statement" vars
    newBindings <- parseBindings bindings variables
    evaluate newBindings expression

-- (lambda (a b c ...) expression)
evaluate bindings (ListE (SymE "lambda":rest)) = do
  (args, expression) <- extract2 "lambda" rest
  arguments <- castListE "`lambda` should be followed by a list of arguments" args
  textArgs <- mapM (castSymE errorMsg) arguments
  return $ LambdaR bindings textArgs expression
    where
      errorMsg = "`lambda` can only have vanilla symbols as arguments"

-- (quote a)
evaluate bindings (ListE (SymE "sym":rest)) = do
  atom <- extract1 "sym" rest
  case atom of
    ListE l -> return $ UndefinedR "`sym` can only be applied on symbols"
    atom    -> return $ SymbolR atom

evaluate bindings (ListE []) = return $ ListR []

-- Macro and function calls
evaluate bindings (ListE (functionExpr:args)) = do
  function <- evaluate bindings functionExpr
  case function of
    all@(LambdaR _ _ _) -> curry all args
    all@(MacroR b expr) -> do
                  newAST <- evaluate (insertM "ast" (quote $ ListE args) b) expr
                  let unQuotedAST = unquote newAST
                  evaluate bindings unQuotedAST
    all@(UndefinedR _)  -> return all
    otherwise           -> return $ UndefinedR $ "cannot execute expression `" ++ show function ++ "`"
    where
      curry (LambdaR oldB [] expression) [] = evaluate oldB expression
      curry lambda []                       = return lambda
      curry (LambdaR oldB (formalA:formalAs) expr) (arg:args) =
          do
            let eArg = ThunkR bindings arg
            curry (LambdaR (insertM formalA eArg oldB) formalAs expr) args
      curry _ _ = error "curry should only be evaluated after proper argument checking!"
      quote (ListE l)    = ListR $ map quote l
      quote x            = SymbolR x

-- Parse bindings from a list like ((var0 exp0) (var1 exp1) ...)
parseBindings = foldM addBindings
  where
    addBindings oldMap (ListE [SymE var, e]) = do
      let generator = \map -> insertM var (ThunkR map e) oldMap
      return $ fix generator
    addBindings _ list = EResult $ "invalid binding syntax: `" ++ show list ++ "`"

builtins = let evaluated = do
                 exps <- fullParse origin
                 parseBindings emptyM $ reverse exps
           in case evaluated of
                CResult exps -> exps
                EResult str  -> error $ "evaluating Origin failed because of error " ++ str

-- Unquotes a Result
unquote :: Result -> Exp
unquote (IntegerR i) = IntegerE i
unquote (BooleanR b) = BooleanE b
unquote (SymbolR x)  = x
unquote (ListR l)    = ListE $ map unquote l
unquote _            = error "unquoting arbitrary values is a sin!"

evalTopLevel :: Bindings -> Exp -> MResult String (Bindings, Result)
-- (defun foo bar baz) == (set foo (Y (lambda foo bar baz)))
evalTopLevel bindings (ListE (SymE "defun":rest)) = do
  (name, args, expr) <- extract3 "defun" rest
  nameText <- castSymE "a `defun` needs to have a symbol as its name" name
  arguments <- castListE "the second argument to a `defun` is an argument list" args
  let lambda = ListE [SymE "lambda", ListE (name:arguments), expr]
  let recursiveL = ListE [SymE "Y", lambda]
  value <- evaluate bindings recursiveL
  return (insertM nameText value bindings, SymbolR name)

evalTopLevel bindings (ListE (SymE "defmacro":rest)) = do
  (name, expr) <- extract2 "defmacro" rest
  nameText <- castSymE "a `defmacro` needs to have a symbol as its name" name
  let macro = MacroR bindings expr
  return (insertM nameText macro bindings, SymbolR name)

evalTopLevel bindings expression =  do
  eExpr <- evaluate bindings expression
  return (bindings, eExpr)

mapMContext :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m [c]
mapMContext _ _ [] = return []
mapMContext f a (b:bs) = do
  (newSeed, this) <- f a b
  rest <- mapMContext f newSeed bs
  return (this:rest)

-- Essentially the complete external interface for this module.  Evaluates
-- a string in a fresh context.
eval :: String -> MResult String [Result]
eval s = do
   exps <- fullParse s
   mapMContext evalTopLevel builtins $ reverse exps

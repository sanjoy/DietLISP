module Semantics(evaluate, eval) where

{- Denotational semantics-like reduction rules.  -}

import Lexer
import Parser

import Data.Map(Map, empty, fromList, insert, lookup)

-- Pretty way to prevent clashes.
emptyM  = empty
insertM :: (Ord k) => k -> a -> Map k a -> Map k a
insertM = Data.Map.insert
lookupM :: (Ord k) => k -> Map k a -> Maybe a
lookupM = Data.Map.lookup

type Bindings = Map String Result

-- An expression evaluates to this type.
data Result = UndefinedR | IntegerR Integer | BooleanR Bool | ListR [Result]
            | LambdaR Bindings {- Closure -} [String] {- Args -}  Exp {- Body -}
            | UndefinedStrR String
              deriving(Eq)

-- Prettier display
instance Show Result where
  show UndefinedR        = "⊥"
  show (IntegerR i)      = show i
  show (BooleanR b)      = show b
  show (ListR l)         = show l
  show (LambdaR _ _ _)   = "## Function Object ##"
  show (UndefinedStrR s) = "⊥ (" ++ s ++ ")"

-- Fold OPERANDS (assumed integers) using OPERATION seeding with BEGIN in
-- context BINDINGS.  Result in UndefinedR in case of operands of
-- incorrect type, IntegerR otherwise.
nAryOp bindings operands begin operation =
  let integers = map (evaluate bindings) operands
  in foldl evalOp (IntegerR begin) integers
    where
      evalOp (IntegerR i) (IntegerR j) = (IntegerR $ operation i j)
      evalOp _ _                       =
          UndefinedStrR $ "Attempted to apply integer operation to " ++
                          "non-integer"

-- Combine LEFT and RIGHT (both assumed integers) using OPERATION in
-- context BINDINGS.  Result in UndefinedR in case of operands of
-- incorrect type, IntegerR otherwise.
binaryOp bindings left right operation =
  evalOp (evaluate bindings left) (evaluate bindings right)
    where
      evalOp (IntegerR i) (IntegerR j) = (IntegerR $ operation i j)
      evalOp _ _                       =
          UndefinedStrR $ "Attempted to apply integer operation to " ++
                          "non-integer"

-- Compare LEFT and RIGHT (both assumed integers) using OPERATION in
-- context BINDINGS.  Result in UndefinedR in case of operands of
-- incorrect type, BooleanR otherwise.
relatOp bindings left right operation =
  evalOp (evaluate bindings left) (evaluate bindings right)
    where
      evalOp (IntegerR i) (IntegerR j) = (BooleanR $ operation i j)
      evalOp _ _                       =
          UndefinedStrR $ "Attempted to apply integer operation to " ++
                          "non-integer"

evaluate :: Bindings -> Exp -> Result

evaluate _ (IntegerE i) = IntegerR i
evaluate _ (BooleanE b) = BooleanR b
evaluate _ (SymE "null") = ListR []

evaluate bindings (SymE s) =
    case lookupM s bindings of
      Just result -> result
      Nothing     -> UndefinedStrR $ "Could not resolve symbol `" ++ s ++ "`"

-- Basic arithmetic.
evaluate bindings (ListE (SymE "+":addends))  = nAryOp bindings addends 0 (+)
evaluate bindings (ListE (SymE "*":addends))  = nAryOp bindings addends 1 (*)
evaluate bindings (ListE [SymE "-", a, b])    = binaryOp bindings a b (-)
evaluate bindings (ListE [SymE "/", a, b])    = binaryOp bindings a b div
evaluate bindings (ListE [SymE "%", a, b])    = binaryOp bindings a b mod

-- Relational arithmetic
evaluate bindings (ListE [SymE ">", a, b])    = relatOp bindings a b (>)
evaluate bindings (ListE [SymE ">=", a, b])   = relatOp bindings a b (>=)
evaluate bindings (ListE [SymE "<", a, b])    = relatOp bindings a b (<)
evaluate bindings (ListE [SymE "<=", a, b])   = relatOp bindings a b (<=)

-- (if condition true-value false-value)
-- :: typeof (true-value) == typeof (false-value)
evaluate bindings (ListE [SymE "if", condition, a, b]) =
  evaluateIf (evaluate bindings condition) a b
    where
      evaluateIf (BooleanR True) a _  = evaluate bindings a
      evaluateIf (BooleanR False) _ b = evaluate bindings b
      evaluateIf _                _ _ =
          UndefinedStrR "Condition non-boolean in `if`"

-- (zerop integer)
-- :: Integer -> Bool
-- This is different from (lambda (x) (== x 0)) because it also asserts the
-- type.
evaluate bindings (ListE [SymE "zerop", value]) =
  evaluateZeroP $ evaluate bindings value
    where
      evaluateZeroP (IntegerR 0) = BooleanR True
      evaluateZeroP (IntegerR _) = BooleanR False
      evaluateZeroP _            =
          UndefinedStrR "`zerop` applied on non-integer"

-- (cons head tail)
-- :: ([x], x) -> [x]
evaluate bindings (ListE [SymE "cons", head, tail]) =
  evaluateCons (evaluate bindings head) (evaluate bindings tail)
    where
      consCompatible :: Result -> Result -> Bool
      consCompatible (IntegerR _)  (IntegerR _)  = True
      consCompatible (BooleanR _)  (BooleanR _)  = True
      consCompatible (ListR (a:_)) (ListR (b:_)) = consCompatible a b
      consCompatible _             _             = False

      evaluateCons :: Result -> Result -> Result
      evaluateCons a (ListR []) = ListR [a]
      evaluateCons a (ListR (b:rest)) =
        if consCompatible a b then
            ListR (a:b:rest)
        else
            UndefinedStrR "Incompatible types in `cons`"

-- (head list)
-- :: [x] -> x
evaluate bindings (ListE [SymE "head", list]) =
  evaluateHead $ evaluate bindings list
    where
      evaluateHead (ListR (x:_)) = x
      evaluateHead _             =
          UndefinedStrR "Non-list type in `head`"

-- (tail list)
-- :: [x] -> [x]
evaluate bindings (ListE [SymE "tail", list]) =
  evaluateTail $ evaluate bindings list
    where
      evaluateTail (ListR (_:xs)) = ListR xs
      evaluateTail _              =
          UndefinedStrR "Non-list type in `tail`"

-- (nullp list)
-- :: [x] -> Bool
evaluate bindings (ListE [SymE "nullp", list]) =
  evaluateNullP $ evaluate bindings list
    where
      evaluateNullP (ListR []) = BooleanR True
      evaluateNullP (ListR _)  = BooleanR False
      evaluateNullP _          =
          UndefinedStrR "Non-list type in `nullp`"

-- (== a b)
-- :: x -> x -> Bool
evaluate bindings (ListE [SymE "==", a, b]) =
  let left  = evaluate bindings a
      right = evaluate bindings b
  in if left == UndefinedR || right == UndefinedR
     then UndefinedStrR "`⊥` passed as `==` operand"
     else BooleanR $ left == right

-- (&& a b)
-- :: Bool -> Bool -> Bool (Short-circuted)
evaluate bindings (ListE [SymE "&&", a, b]) =
  let left = evaluate bindings a
  in if left == BooleanR False then BooleanR False else
         -- Force fail if b is not a BooleanR
         let (BooleanR x) = evaluate bindings b
         in BooleanR x

-- (|| a b)
-- :: Bool -> Bool -> Bool (Short-circuited)
evaluate bindings (ListE [SymE "||", a, b]) =
  let left = evaluate bindings a
  in if left == BooleanR True then BooleanR True else
         -- Force fail if b is not a BooleanR
         let (BooleanR x) = evaluate bindings b
         in BooleanR x

-- (let ((var0 value0) (var1 value1) ...) expression)
-- :: typeof (expression)
evaluate bindings (ListE [SymE "let", ListE variables, expression]) =
  let newBindings = foldl addBindings bindings variables
  in evaluate newBindings expression
    where
      addBindings oldMap (ListE [SymE v, e]) =
        insertM v (evaluate oldMap e) oldMap

-- (lambda (a b c ...) expression)
evaluate bindings (ListE [SymE "lambda", ListE args, expression]) =
  LambdaR bindings (map extract args) expression
    where
      extract (SymE s) = s

-- (lambda arg0 arg1 ...)
evaluate bindings (ListE generic) =
  let function = evaluate bindings $ head generic
      args = tail generic
  in
    curry function args
      where
        curry (LambdaR oldB [] expression) [] = evaluate oldB expression
        curry lambda []                       = lambda
        curry (LambdaR oldB (a:as) expr) (e:es) =
          curry (LambdaR (insertM a (evaluate bindings e) oldB) as expr) es
        curry _ _ = UndefinedStrR "Non-function called"

evalInternal = (evaluate emptyM) . parse . tokenize

-- Some features built into the interperter for convenience.
idExp   = evalInternal $ "(lambda (x) x)"
yExp    = evalInternal $ "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))"
notExp  = evalInternal $ "(lambda (x) (if (== x true) false true))"
succExp = evalInternal $ "(lambda (n f x) (f (n f x)))"
zeroExp = evalInternal $ "(lambda (f x) x)"

builtins = fromList [("id", idExp),
                     ("Y", yExp),
                     ("not", notExp),
                     ("cSucc", succExp),
                     ("cZero", zeroExp)]

-- Essentially the complete external interface for this module.  Evaluates
-- a string in a fresh context.
eval :: String -> Result
eval = (evaluate builtins) . parse . tokenize
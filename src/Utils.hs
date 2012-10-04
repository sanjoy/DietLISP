module Utils(ErrorM(..), reportError, extract1, extract2, extract3,
            correctZip) where

data ErrorM a = Error String | Result a

instance Monad ErrorM where
  return            = Result
  (Result a) >>= f  = f a
  (Error str) >>= f = Error str

reportError = Error
instance (Show a) => Show (ErrorM a) where
  show (Result a) = show a
  show (Error str) = "error: " ++ str

argsError name n = reportError $ "`" ++ name ++ "` expects only " ++ n ++ " argument" ++
                   (if name == "one" then "" else "s")

extract1 :: String -> [a] -> ErrorM a
extract1 _ [x]  = return x
extract1 name _ = argsError name "one"

extract2 :: String -> [a] ->  ErrorM (a, a)
extract2 _ [x, y] = return (x, y)
extract2 name _   = argsError name "two"

extract3 :: String -> [a] -> ErrorM (a, a, a)
extract3 _ [x, y, z] = return (x, y, z)
extract3 name _      = argsError name "three"

correctZip :: [a] -> [b] -> Maybe [(a, b)]
correctZip as bs = correctZipRecurse [] as bs
  where correctZipRecurse result [] [] = Just $ reverse result
        correctZipRecurse result (a:as) (b:bs) =
          correctZipRecurse ((a, b):result) as bs
        correctZipRecurse _ _ _ = Nothing

module Utils(MResult(..)) where

data MResult a b = EResult a | CResult b

instance Monad (MResult a) where
  return           = CResult
  (CResult a) >>= f  = f a
  (EResult str) >>= f = EResult str

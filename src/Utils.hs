module Utils where

instance Monad (Either a) where
  return           = Right
  (Right a) >>= f  = f a
  (Left str) >>= f = Left str

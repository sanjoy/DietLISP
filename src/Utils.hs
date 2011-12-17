module Utils(mapContext) where

-- Utility function for mapping with some context
mapContext :: (a -> b -> (a, c)) -> a -> [b] -> [c]
mapContext f a [] = []
mapContext f a (x:xs) = let (ctx, value) = f a x
                        in (value:mapContext f ctx xs)

instance Monad (Either a) where
  return           = Right
  (Right a) >>= f  = f a
  (Left str) >>= f = Left str

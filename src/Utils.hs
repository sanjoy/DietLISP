module Utils(mapContext) where

-- Utility function for mapping with some context
mapContext :: (a -> b -> (a, c)) -> a -> [b] -> [c]
mapContext f a [] = []
mapContext f a (x:xs) = let (ctx, value) = f a x
                        in (value:mapContext f ctx xs)

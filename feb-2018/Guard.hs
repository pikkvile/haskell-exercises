module Guard where

import Control.Monad (guard)

filtered = [x * x * x | x <- [1..10], x * x < 10]

filtered' = [1..10] >>= (\x -> guard (x * x < 10) >> [x * x * x])

filtered'' = do
    x <- [1 .. 10]
    guard (x * x < 10)
    return (x * x * x)
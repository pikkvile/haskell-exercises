module Chess2 where

import Control.Monad (guard, (<=<))

type Pos = (Int, Int)
onBoard :: Pos -> Bool
onBoard (x, y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

move :: Pos -> [Pos]
move (x, y) = do
    next <- [(x + 2, y + 1), (x + 2, y - 1), (x + 1, y + 2), (x + 1, y - 2),
             (x - 2, y + 1), (x - 2, y - 1), (x - 1, y + 2), (x - 1, y - 2)]
    guard (onBoard next)
    return next

moves :: Int -> Pos -> [Pos]
moves n = foldr (<=<) return (replicate n move)
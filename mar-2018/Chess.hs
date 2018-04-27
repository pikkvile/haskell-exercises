module Chess where

import Control.Monad (foldM, (<=<), guard)

type Pos = (Int, Int)

onBoard :: Pos -> Bool
onBoard (x, y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

moveKnight' :: Int -> Pos -> [Pos]
moveKnight' n = foldr (<=<) return (replicate n moveKnight)

moveKnight :: Pos -> [Pos]
moveKnight (x, y) = do
    next <- [(x + 1, y + 2), (x + 1, y - 2), (x + 2, y + 1), (x + 2, y - 1),
             (x - 1, y + 2), (x - 1, y - 2), (x - 2, y + 1), (x - 2, y - 1)]
    guard (onBoard next)
    return next
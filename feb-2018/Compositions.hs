module Compositions where

import Control.Monad (guard, (<=<))
import Data.List (group, sort)

type Pos = (Int, Int)
onBoard :: Pos -> Bool
onBoard (x, y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

moveK :: Pos -> [Pos]
moveK (x, y) = do
    next <- [(x + 1, y + 2), (x + 1, y - 2), (x + 2, y + 1), (x + 2, y - 1),
             (x - 1, y + 2), (x - 1, y - 2), (x - 2, y + 1), (x - 2, y - 1)]
    guard (onBoard next)
    return next

moveKs :: Int -> Pos -> [Pos]
moveKs n start = removeDuplicates $ return start >>= foldr (<=<) return (replicate n moveK)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort
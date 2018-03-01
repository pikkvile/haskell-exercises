module Compositions2 where

import Control.Monad (guard, (<=<))
import Data.List (group, sort)

type Pos = (Int, Int)
onBoard :: Pos -> Bool
onBoard (x,y) = 1 <= x && x <= 8 && 1 <= y && y <= 8

moveKnightNTimes :: Int -> Pos -> [Pos]
moveKnightNTimes n = foldr (<=<) return (replicate n moveKnight)

moveKnight :: Pos -> [Pos]
moveKnight (x,y) = do
    move <- [(x + 2, y + 1), (x + 2, y - 1), (x + 1, y + 2), (x + 1, y - 2),
             (x - 2, y + 1), (x - 2, y - 1), (x - 1, y + 2), (x - 1, y - 2)]
    guard (onBoard move)
    return move

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort
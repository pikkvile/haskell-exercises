module Jan24 where

import Control.Monad (guard)

type Pos = (Int, Int)

onBoard :: Pos -> Bool
onBoard (x,y) = x > 0 && x < 9 && y > 0 && y < 9

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr (\el acc -> if (el `elem` acc) then acc else el : acc) []

knightPossibleMovesFrom :: Pos -> [Pos]
knightPossibleMovesFrom (x,y) = do
    pos'@(x', y') <- [(x - 2, y - 1), (x - 2, y + 1), (x - 1, y - 2), (x - 1, y + 2),
                      (x + 2, y - 1), (x + 2, y + 1), (x + 1, y - 2), (x + 1, y + 2)]
    guard (onBoard pos')
    return pos'

knightPossiblePathsFrom :: [Pos] -> [[Pos]]
knightPossiblePathsFrom path@((x,y) : _) = do
    pos'@(x', y') <- [(x - 2, y - 1), (x - 2, y + 1), (x - 1, y - 2), (x - 1, y + 2),
                      (x + 2, y - 1), (x + 2, y + 1), (x + 1, y - 2), (x + 1, y + 2)]
    guard (onBoard pos')
    return (pos' : path)

knightPossiblePositionsAfter3Moves :: Pos -> [Pos]
knightPossiblePositionsAfter3Moves start = removeDuplicates (do
    afterFirst <- knightPossibleMovesFrom start
    afterSecond <- knightPossibleMovesFrom afterFirst
    knightPossibleMovesFrom afterSecond)

knightPossiblePathsIn3Moves :: [Pos] -> [[Pos]]
knightPossiblePathsIn3Moves start = removeDuplicates (do
    afterFirst <- knightPossiblePathsFrom start
    afterSecond <- knightPossiblePathsFrom afterFirst
    knightPossiblePathsFrom afterSecond)

canReachIn3MovesFrom :: Pos -> Pos -> Bool
canReachIn3MovesFrom from to = to `elem` (knightPossiblePositionsAfter3Moves from)

paths3From :: Pos -> Pos -> [[Pos]]
paths3From from to = filter fits (knightPossiblePathsIn3Moves [from])
    where
        fits :: [Pos] -> Bool
        fits (p : _) = p == to

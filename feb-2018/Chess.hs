module Chess where

import Control.Monad (guard)

type Pos = (Int, Int)
type Path = [Pos]

knight3PathsFrom :: Pos -> [Path]
knight3PathsFrom start = do
    first <- knightPathsFrom start
    second <- knightPathsAfter first
    knightPathsAfter second

knightMovesFrom :: Pos -> [Pos]
knightMovesFrom from@(x, y) = do
    to@(xc, yc) <- [(x + 2, y + 1), (x + 2, y - 1), (x - 2, y - 1), (x - 2, y + 1),
                  (y + 2, x + 1), (y + 2, x - 1), (y - 2, x - 1), (y - 2, x + 1)]
    guard (xc >= 1 && xc <= 8 && yc >= 1 && yc <= 8)
    return to

knightPathsFrom :: Pos -> [Path]
knightPathsFrom from = do
    move <- knightMovesFrom from
    return [move, from]

knightPathsAfter :: Path -> [Path]
knightPathsAfter path@(last : _) = do
    move <- knightMovesFrom last
    return (move : path)
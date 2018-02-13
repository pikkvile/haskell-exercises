module Jan30 where

import Control.Monad (guard)

type Pos = (Int, Int)
type Path = [Pos]

onDeck :: Pos -> Bool
onDeck (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr (\x acc -> if (x `elem` acc) then acc else x : acc) []

movesFrom :: Pos -> [Pos]
movesFrom (x, y) = do
    candidate <- [(x + 1, y + 2), (x + 2, y + 1), (x + 2, y - 1), (x + 1, y - 2),
                  (x - 1, y + 2), (x - 2, y + 1), (x - 2, y - 1), (x - 1, y - 2)]
    guard (onDeck candidate)
    return candidate

pathsFrom :: Path -> [Path]
pathsFrom path@(stop:_) = map (\to -> to : path) (movesFrom stop)

move3 :: Pos -> Pos -> [Path]
move3 from to = removeDuplicates (do
    first <- pathsFrom [from]
    second <- pathsFrom first
    third <- pathsFrom second
    guard ((head third) == to)
    return third)

applyWriter :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyWriter (v, log) f = let (v', log') = f v in (v', log `mappend` log')

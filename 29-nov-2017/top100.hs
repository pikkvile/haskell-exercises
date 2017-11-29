import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C

type Counter = M.Map String Int

main = interact top100

top100 :: String -> String
top100 = unlines .
    (map show) .
    (take 100) .
    (L.sortBy countSort) .
    M.toList .
    countWords .
    contentWords

contentWords :: String -> [String]
contentWords = (map norm) . (concatMap words) . lines

countWords :: [String] -> Counter
countWords words = foldl counterUpdate M.empty (map (\w -> (w, 1)) words)

counterUpdate :: Counter -> (String, Int) -> Counter
counterUpdate counter (word, count) = case M.lookup word counter of
    Nothing -> M.insert word count counter
    Just c -> M.insert word (c + count) counter

countSort :: (String, Int) -> (String, Int) -> Ordering
countSort (_, i1) (_, i2) = if (i1 < i2) then GT else LT

norm :: String -> String
norm = map C.toLower
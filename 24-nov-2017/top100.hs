import Control.Monad
import Data.Map as Map hiding (foldl, map)
import Data.List

main = do
    content <- getContents
    mapM_ putStrLn (map show (top 10 content))

top :: Int -> String -> [(String, Int)]
top n s = take n $ sortBy srt (toList (foldl cnt Map.empty (map (\w -> (w, 1)) (concatMap words (lines s)))))

cnt :: Map String Int -> (String, Int) -> Map String Int
cnt m (k, v) = Map.insert k (case Map.lookup k m of
    Just ev -> ev + v
    Nothing -> v) m

srt :: (String, Int) -> (String, Int) -> Ordering
srt (_, i1) (_, i2) = if (i1 < i2) then GT else LT
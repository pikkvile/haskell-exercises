import System.Environment
import Control.Monad
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C

main = do
    (fileName : topSizeStr : []) <- getArgs
    contents <- readFile fileName
    forM_ (top (read topSizeStr) contents) (putStrLn . show)

type Counter = M.Map String Int

top :: Int -> String -> [(String, Int)]
top i = take i . L.sortBy ord . M.toList . foldl cnt M.empty . map (\w -> (w, 1)) . words'

words' :: String -> [String]
words' = map (map C.toLower) . map (filter (\c -> not $ elem c ".,:;-_'\"<>/")) . words

cnt :: Counter -> (String, Int) -> Counter
cnt counter (word, c) = case M.lookup word counter of
    Just cEx -> M.insert word (cEx + c) counter
    Nothing -> M.insert word c counter

ord :: (String, Int) -> (String, Int) -> Ordering
ord a b = if ((snd a) > (snd b)) then LT else GT

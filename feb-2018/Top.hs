module Top where

import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import Data.Char (isLetter, toLower)
import qualified Data.Map as M
import qualified Data.List as L

parse :: [String] -> Maybe (String, Int)
parse (f : s : []) = Just (f, read s)
parse _ = Nothing

normalize :: String -> Maybe String
normalize w = let w' = map toLower (filter isLetter w) in
    if length w' > 0 then Just w' else Nothing

extractWords :: String -> [String]
extractWords = catMaybes . (map normalize) . words

findTop :: FilePath -> Int -> IO [(String, Int)]
findTop f s = do
    counted <- fmap (countWords . extractWords) (readFile f)
    return (getTop s counted)

countWords :: [String] -> M.Map String Int
countWords = foldl (\m w -> case M.lookup w m of
    Just c -> M.insert w (c + 1) m
    Nothing -> M.insert w 1 m) M.empty

getTop :: Int -> M.Map String Int -> [(String, Int)]
getTop s = (take s) . (L.sortBy compareSecondDesc) . M.toList
    where
        compareSecondDesc :: (String, Int) -> (String, Int) -> Ordering
        compareSecondDesc (_, i1) (_, i2)
            | i1 > i2 = LT
            | i1 == i2 = EQ
            | i1 < i2 = GT

main :: IO ()
main = do
    input <- fmap parse getArgs
    case input of
        Just (fileName, topSize) -> do
            top <- findTop fileName topSize
            putStrLn (show top)
        Nothing -> putStrLn "Usage: top <filename> <size>"
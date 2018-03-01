module RndGenTest where

import qualified Data.Map as M
import System.Random (getStdGen, randomRs)

count :: Ord a => [a] -> M.Map a Int
count = foldl (\m x -> case M.lookup x m of
    Just c -> M.insert x (c + 1) m
    Nothing -> M.insert x 1 m) M.empty

randomsList :: Int -> IO (M.Map Int Int)
randomsList size = do
    gen <- getStdGen
    let list = take size (randomRs (1,5) gen)
    return (count list)
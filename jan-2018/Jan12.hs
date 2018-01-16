module Jan12 where

import qualified Data.Map as M

type Counter a = M.Map a (Int, Double)

fractions :: (Ord a) => [a] -> Counter a
fractions coll = let counts = foldl count M.empty coll in M.map (calculateFraction (total counts)) counts

count :: (Ord a) => M.Map a Int -> a -> M.Map a Int
count counter el = case M.lookup el counter of
    Just count -> M.insert el (count + 1) counter
    Nothing -> M.insert el 1 counter

calculateFraction :: Int -> Int -> (Int, Double)
calculateFraction total count = (count, (fromIntegral count) / (fromIntegral total))

total :: M.Map a Int -> Int
total = M.foldl' (+) 0
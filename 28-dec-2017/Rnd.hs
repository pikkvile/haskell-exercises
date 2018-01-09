module Rnd where

import System.Random
import Control.Monad

main = do
    gen <- getStdGen
    mapM_ putStrLn (map show (testStdRnd (1,5) gen))

testSize :: Int
testSize = 1000000

testNums :: Int -> [Int]
testNums n = replicate testSize n

testRandoms :: (Int, Int) -> StdGen -> [Int]
testRandoms bounds gen = take testSize $ randomRs bounds gen

eqs :: Int -> Int -> Int
eqs a b = if (a == b) then 1 else 0

test :: (Int, Int) -> StdGen -> Int -> Double
test bounds gen n = (fromIntegral (foldl1 (+) (zipWith eqs (testNums n) (testRandoms bounds gen)))) / (fromIntegral testSize)

testStdRnd :: (Int, Int) -> StdGen -> [Double]
testStdRnd bounds@(from, to) gen = map (test bounds gen) [from .. to]
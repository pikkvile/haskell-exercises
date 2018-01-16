module Jan15 where

import Control.Applicative
import System.Random

import Jan12

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

sequenceA'' :: Applicative f => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])

main = do
    con <- (++) <$> getLine <*> getLine
    putStrLn con

testStdGen :: IO ()
testStdGen = do
    gen <- getStdGen
    putStrLn (show (Jan12.fractions (take 10000 (randomRs (1 :: Int, 10 :: Int) gen))))
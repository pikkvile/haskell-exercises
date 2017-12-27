module Guess where

import System.Random
import Control.Monad

main = do
    gen <- getStdGen
    interactGuess gen

interactGuess :: StdGen -> IO ()
interactGuess gen = do
    guessStr <- getLine
    when (not $ null guessStr) $ do
        let (rnd, newGen) = randomR (1, 5) gen
            guess = read guessStr :: Int
            result = if (guess == rnd) then "YES!" else "NO..."
        putStrLn result
        interactGuess newGen

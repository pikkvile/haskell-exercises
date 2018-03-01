module GuessRandom where

import System.Random (getStdGen, randomR, StdGen)

main :: IO ()
main = do
    gen <- getStdGen
    guess gen

guess :: StdGen -> IO ()
guess gen = do
    let (num, gen') = randomR (1,5) gen
    putStrLn "Guess a number:"
    g <- getInt
    if (num == g)
        then putStrLn "OK!"
        else do
            putStrLn "Nok..."
            guess gen'

getInt :: IO Int
getInt = fmap read getLine
import Control.Monad
import System.Random

main = do
    gen <- getStdGen
    doGuess gen

doGuess :: StdGen -> IO ()
doGuess gen = do
    guessStr <- getLine
    when (not $ null guessStr) $ do
        let guess = read guessStr :: Int
        let (r, newGen) = randomR (1, 5) gen
        if (guess == r) then putStrLn "Wow! Aren't you Wanga?" else putStrLn "Nope, try again..."
        doGuess newGen

import System.Random
import Control.Monad

main = do
    generator <- getStdGen
    forM_ ((randomRs (1,5) generator) :: [Int]) (\r -> do
        putStrLn "Guess the Number (1-5)"
        numStr <- getLine
        when (not $ null numStr ) $ putStrLn (if (read numStr == r) then "Yeah, you got it!" else "Try again..."))
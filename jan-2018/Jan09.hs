module Jan09 where

import Control.Monad

-- Palindromes
isPal :: String -> Bool
isPal s = s == reverse s

isPalInteract :: IO ()
isPalInteract = do
    line <- getLine
    when (not $ null line) $ do
        putStrLn (show (isPal line))
        isPalInteract

main1 = isPalInteract

-- IO as Functor
main2 = do
    reversed <- fmap reverse getLine
    putStrLn reversed

-- forM, forM_
main3 = forM_ [1..4] readAndDoAndPrint

readAndDoAndPrint :: Int -> IO ()
readAndDoAndPrint n = do
    toAdd <- fmap read getLine
    putStrLn (show (n + toAdd))


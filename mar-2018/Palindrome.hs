module Palindrome where

import Control.Monad (guard)

main :: IO ()
main = do
    input <- getLine
    if (not $ null input)
        then do
            putStrLn (if (input == (reverse input))
                then "Yes"
                else "No")
            main
        else return ()
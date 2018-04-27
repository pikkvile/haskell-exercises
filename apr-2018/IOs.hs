module IOs where

import Control.Monad (forM)

colors :: [String]
colors = ["Red", "Green", "Blue"]

getColorAssociation :: String -> IO String
getColorAssociation color = do
    putStrLn ("How about " ++ color ++ "?")
    answer <- getLine
    return (color ++ " -> " ++ answer)

main = do
    answers <- forM colors getColorAssociation
    putStrLn (show answers)
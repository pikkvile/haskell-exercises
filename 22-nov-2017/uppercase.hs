import Data.Char

main = do
    line <- getLine
    putStrLn (map toUpper line)
module Printer where

import System.Environment

main = do
    (fileName:[]) <- getArgs
    contents <- readFile fileName
    putStrLn contents
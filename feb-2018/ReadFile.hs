{-# LANGUAGE ScopedTypeVariables #-}

module ReadFile where

import Control.Exception (handle, IOException)

main = do
    fileName <- getLine
    handle (\(e :: IOException) -> putStrLn ("Something wrong with this file: " ++ show e)) $ do
        contents <- readFile fileName
        putStrLn contents
module Jan10 where

import System.IO
import Control.Exception
import System.Environment

-- bracket

sumFirstLine :: String -> Int
sumFirstLine = foldl (+) 0 . map read . words

openAndPrint :: String -> IO ()
openAndPrint fileName = bracket
    (openFile fileName ReadMode)
    (\h -> do
        hClose h
        putStrLn "closed")
    (\h -> do
        contents <- hGetContents h
        putStrLn (show (sumFirstLine contents)))

main = do
    (fileName : []) <- getArgs
    openAndPrint fileName

-- tree (functor, foldable)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Foldable Tree where
    foldMap _ EmptyTree = mempty
    foldMap f (Node x left right) = (foldMap f left) `mappend` (f x) `mappend` (foldMap f right)
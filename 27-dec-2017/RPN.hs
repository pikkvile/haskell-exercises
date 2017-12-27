module RPN where

import Control.Monad

main = interactEval

interactEval :: IO ()
interactEval = do
  expr <- getLine
  when (not $ null expr) $ do
    putStrLn (eval expr)
    interactEval

eval :: String -> String
eval expr = show . head . foldl f [] $ (words expr)
    where
        f :: [Double] -> String -> [Double]
        f (a : b : stack) "+" = (a + b) : stack
        f (b : a : stack) "-" = (a - b) : stack
        f (a : b : stack) "*" = (a * b) : stack
        f (b : a : stack) "/" = (a / b) : stack
        f stack num = (read num) : stack

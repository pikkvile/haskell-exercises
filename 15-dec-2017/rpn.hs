import Control.Monad

main = interact' ev

interact' :: (String -> String) -> IO ()
interact' f = do
    line <- getLine
    when (not $ null line) $ do
        putStrLn (ev line)
        interact' f

ev :: String -> String
ev = show . head . foldl rpn [] . words

rpn :: [Double] -> String -> [Double]
rpn (x : y : stack) "+" = (x + y) : stack
rpn (y : x : stack) "-" = (x - y) : stack
rpn (x : y : stack) "*" = (x * y) : stack
rpn (y : x : stack) "/" = (x / y) : stack
rpn stack num = (read num) : stack
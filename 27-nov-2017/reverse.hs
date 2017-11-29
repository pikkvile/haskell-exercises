import Control.Monad

main = do
    line <- getLine
    when (not $ null line) $ do
        putStrLn (reverse line)
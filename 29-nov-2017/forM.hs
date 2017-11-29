import Control.Monad

main = forM_ [1,2,3,4] readPlus

readPlus :: Int -> IO ()
readPlus i = do
    inp <- getLine
    let j = read inp :: Int
    putStrLn ((show i) ++ " + " ++ inp ++ " = " ++ (show (i + j)))
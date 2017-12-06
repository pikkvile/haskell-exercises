import System.Environment

main = do
    args <- getArgs
    putStrLn ("args: " ++ show args)
    prog <- getProgName
    putStrLn ("prog: " ++ prog)
import System.Environment
import System.IO

main = do
    (fileName : []) <- getArgs
    withFile fileName ReadMode (\h -> do
        c <- hGetContents h
        putStrLn c)
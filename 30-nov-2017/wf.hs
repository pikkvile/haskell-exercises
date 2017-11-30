import System.IO

main = do
    withFile "/home/idubov/txt/sc.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
import System.IO
import Control.Exception

main = bracket
    (openFile "/home/idubov/txt/sc.txt" ReadMode)
    (hClose)
    (\handle -> do
        contents <- hGetContents handle
        putStr contents)
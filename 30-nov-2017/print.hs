import System.IO

main = do
    handle <- openFile "/home/idubov/txt/sc.txt" ReadMode
    c <- hGetContents handle
    putStr c
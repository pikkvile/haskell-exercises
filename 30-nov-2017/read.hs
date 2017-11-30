import System.IO

main = do
    c <- readFile "/home/idubov/txt/sc.txt"
    putStr c
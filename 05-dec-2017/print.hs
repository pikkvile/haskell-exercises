import System.IO

main = do
    h <- openFile "/tmp/todo.txt" ReadMode
    c <- hGetContents h
    putStr c
    hClose h
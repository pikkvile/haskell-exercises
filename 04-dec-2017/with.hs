import System.IO

main = withFile "/tmp/todo.txt" ReadMode (\h -> do
    c <- hGetContents h
    putStrLn c)
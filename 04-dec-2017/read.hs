main = do
    c <- readFile "/tmp/todo.txt"
    putStrLn c
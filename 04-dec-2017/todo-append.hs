import Control.Monad

main = forever $ do
    input <- getLine
    appendFile "/tmp/todo.txt" (input)
    putStrLn "ok"
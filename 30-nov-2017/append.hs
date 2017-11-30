import System.IO

main = do
    newTodo <- getContents
    appendFile "/tmp/todo.txt" newTodo
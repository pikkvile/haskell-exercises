import System.Environment
import Data.List
import System.Directory
import System.IO

main = do
    args <- getArgs
    go args

todoFile = "/tmp/todo.txt"

idx :: [String] -> [String]
idx = zipWith (\i s -> show i ++ " - " ++ s) [1 .. ]

printTodos :: IO ()
printTodos = do
    todos <- readFile todoFile
    putStr (unlines . idx . lines $ todos)

go :: [String] -> IO ()
go ["show"] = printTodos
go ["add", newTodo] = do
    appendFile todoFile (newTodo ++ "\n")
    printTodos
go ["delete", numStr] = do
    let num = read numStr :: Int
    todosStr <- readFile todoFile
    let todos = lines todosStr
        newTodos = delete (todos !! (num - 1)) todos
    (tName, tHandle) <- openTempFile "/tmp" "tmp"
    hPutStr tHandle (unlines newTodos)
    hClose tHandle
    removeFile todoFile
    renameFile tName todoFile
    putStr (unlines . idx $ newTodos)

go _ = putStrLn "Unknown command"

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
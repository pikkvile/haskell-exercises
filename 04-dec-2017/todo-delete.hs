import Control.Monad
import System.Directory
import System.IO
import Data.List

main = forever $ do
    contents <- readTodos
    let todos = lines contents
        todosIndexed = indexed todos
    putStrLn (unlines todosIndexed)
    numToDeleteStr <- getLine
    let numToDelete = read numToDeleteStr :: Int
    doDelete numToDelete todos
    putStrLn "ok"

readTodos :: IO String
readTodos = readFile "/tmp/todo.txt"

doDelete :: Int -> [String] -> IO ()
doDelete n todos = do
    let newTodos = delete (todos !! (n - 1)) todos
    (tempName, tempHandle) <- openTempFile "/tmp" "temp"
    hPutStr tempHandle (unlines newTodos)
    hClose tempHandle
    removeFile "/tmp/todo.txt"
    renameFile tempName "/tmp/todo.txt"

nums = [1 .. ]

index :: Int -> String -> String
index i s = (show i) ++ " - " ++ s

indexed :: [String] -> [String]
indexed = zipWith index nums
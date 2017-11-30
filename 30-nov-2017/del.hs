import System.IO
import qualified System.Directory
import qualified Data.List
import Control.Monad

main = forever $ do
    c <- readFile "/tmp/todo.txt"
    let todos = lines c
        numberedTodos = addNums todos
    putStrLn "TODOs:"
    putStr (unlines numberedTodos)
    putStrLn "Which one to delete?"
    idxStr <- getLine
    let idx = read idxStr
        newTodos = Data.List.delete (todos !! (idx - 1)) todos
    (tempName, tempHandle) <- openTempFile "/tmp" "temp"
    hPutStr tempHandle (unlines newTodos)
    hClose tempHandle
    System.Directory.removeFile "/tmp/todo.txt"
    System.Directory.renameFile tempName "/tmp/todo.txt"

addNum :: Int -> String -> String
addNum i s = show i ++ ". " ++ s

addNums :: [String] -> [String]
addNums = (zipWith addNum [1 .. ])
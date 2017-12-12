import System.Environment
import Data.List
import System.IO
import System.Directory

main = do
    args <- getArgs
    todo args
    printIndexedTodos

todoFile = "/home/idubov/todoh.txt"

todo :: [String] -> IO ()
todo ("add" : newItem : []) = appendFile todoFile (newItem ++ "\n")
todo ("show" : []) = return ()
todo ("del" : idxStr : []) = deleteByIndex ((read idxStr) - 1)
todo _ = error "Unknown command"

deleteByIndex :: Int -> IO ()
deleteByIndex i = do
    contents <- readFile todoFile
    (tName, tHandle) <- openTempFile "/tmp" "todoh"
    let todos = lines contents
        newTodos = delete (todos !! i) todos
    hPutStr tHandle (unlines newTodos)
    hClose tHandle
    removeFile todoFile
    renameFile tName todoFile

indexContents :: [String] -> [String]
indexContents = zipWith (\i l -> (show i) ++ " - " ++ l) [1 .. ]

printIndexedTodos :: IO ()
printIndexedTodos = do
    contents <- readFile todoFile
    if (null contents)
        then putStrLn "-- No todos yet! --"
        else putStr (unlines . indexContents . lines $ contents)
module Todo where

import System.Environment
import System.Directory
import System.IO
import qualified Data.List as L

todoFile = "/tmp/todo.txt"

main = do
    createFileIfNotExists
    args <- getArgs
    todo args
    todos <- todoList
    putStr todos

todo :: [String] -> IO ()
todo ("a" : task : []) = appendFile todoFile (task ++ "\n")
todo ("l" : []) = return ()
todo ("d" : taskIdx : []) = deleteByIndex ((read taskIdx) - 1)
todo _ = putStrLn "usage: todo a <task> | todo l | todo d <idx>"

todoList :: IO String
todoList = do
    c <- readFile todoFile
    return $ unlines (zipWith (\i l -> (show i) ++ " - " ++ l) [1..] (lines c))

deleteByIndex :: Int -> IO ()
deleteByIndex idx = do
    (tName, tHandle) <- openTempFile "/tmp" "todotmp.txt"
    c <- readFile todoFile
    let todoLines = lines c
        updatedLines = L.delete (todoLines !! idx) todoLines
    hPutStr tHandle (unlines updatedLines)
    hClose tHandle
    removeFile todoFile
    renameFile tName todoFile

createFileIfNotExists :: IO ()
createFileIfNotExists = do
    fileExists <- doesFileExist todoFile
    if (fileExists)
        then return ()
        else writeFile todoFile ""
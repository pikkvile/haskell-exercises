module Todo where

import System.Environment (getArgs)
import qualified Data.List
import qualified System.IO
import qualified System.Directory

main = do
    params <- getArgs
    process params
    display

process :: [String] -> IO ()
process ("add" : todo : _) = add (todo ++ "\n")
process ("show" : _) = return ()
process ("done" : idx : _) = done (read idx)

todofile = "/home/idubov/txt/todoha/todo.txt"

add :: String -> IO ()
add = appendFile todofile

done :: Int -> IO ()
done idx = do
    content <- readFile todofile
    let todoLines = lines content
        toDelete = todoLines !! (idx - 1)
        newTodoLines = Data.List.delete toDelete todoLines
    (tmpName, tmpHandle) <- System.IO.openTempFile "/home/idubov/txt/todoha" "tmp"
    System.IO.hPutStr tmpHandle (unlines newTodoLines)
    System.IO.hClose tmpHandle
    System.Directory.removeFile todofile
    System.Directory.renameFile tmpName todofile


display :: IO ()
display = do
    content <- readFile todofile
    let todoLines = lines content
        indexed = zipWith (\i l -> (show i) ++ ". " ++ l) [1 .. ] todoLines
    putStr (unlines indexed)
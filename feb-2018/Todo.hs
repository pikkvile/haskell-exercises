module Todo where

import Data.List (delete)
import System.Environment (getArgs)
import System.IO (openTempFile, hPutStr, hClose)
import System.Directory (renameFile, removeFile, doesFileExist)

data Command = Create String | List | Delete Int deriving Show
type Todos = [String]

execute :: Command -> Todos -> Todos
execute (Create body) todos = todos ++ [body]
execute (List) todos = todos
execute (Delete idx) todos = delete (todos !! (idx - 1)) todos

parseArgs :: [String] -> Maybe Command
parseArgs ("new" : body : []) = Just (Create body)
parseArgs ("list" : []) = Just List
parseArgs ("del" : idx : []) = Just (Delete (read idx))
parseArgs _ = Nothing

display :: Todos -> [String]
display = zipWith (\i todo -> (show i) ++ ") " ++ todo) [1..]

file = "/tmp/todos.txt"

loadFromFile :: IO Todos
loadFromFile = fmap lines (readFile file)

writeToFile :: Todos -> IO ()
writeToFile todos = do
    (tmpName, tmpH) <- openTempFile "/tmp" "tmp"
    hPutStr tmpH (unlines todos)
    hClose tmpH
    removeFile file
    renameFile tmpName file

createFileIfNotExists :: IO ()
createFileIfNotExists = do
    exists <- doesFileExist file
    if exists then return () else writeFile file ""

main :: IO ()
main = do
    command <- fmap parseArgs getArgs
    case command of
        Just c -> do
            createFileIfNotExists
            todos <- loadFromFile
            let newTodos = execute c todos
            writeToFile newTodos
            putStr . unlines . display $ newTodos
        Nothing -> putStrLn "Usage: todos [new <todo> | list | del <todo index>]"
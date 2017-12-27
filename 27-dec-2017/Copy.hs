import Data.ByteString.Lazy as BSL
import System.Environment

main = do
    (from : to : []) <- getArgs
    cp from to

cp :: String -> String -> IO ()
cp from to = do
    contents <- BSL.readFile from
    BSL.writeFile to contents
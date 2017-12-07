import System.Environment
import qualified Data.ByteString.Lazy as LS

main = do
    (src : dest : _) <- getArgs
    cp src dest

cp :: String -> String -> IO ()
cp src dest = do
    contents <- LS.readFile src
    LS.writeFile dest contents
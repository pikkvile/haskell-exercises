import qualified Data.ByteString.Lazy as DBL
import System.Environment

main = do
    (from : to : []) <- getArgs
    contents <- DBL.readFile from
    DBL.writeFile to contents
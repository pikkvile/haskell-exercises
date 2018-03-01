module CopyFile where

import System.Environment (getArgs)
import System.IO

import qualified Data.ByteString.Lazy as BSL

main = do
    (from:to:[]) <- getArgs
    fromHandle <- openFile from ReadMode
    contents <- BSL.hGetContents fromHandle
    toHandle <- openFile to WriteMode
    BSL.hPutStr toHandle contents
    hClose fromHandle
    hClose toHandle
module FS2 where

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving Show

testFS = Folder "root" [
    File "test.txt" "test",
    File "wtf.txt" "wrd",
    Folder "sub-folder" [
        File "wtf2.txt" "wtf2"
    ]]

data Crumb = Crumb Name [FSItem] [FSItem] deriving Show
type Zipper = (FSItem, [Crumb])
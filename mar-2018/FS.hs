module FS where

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving Show
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving Show
type FSZipper = (FSItem, [FSCrumb])

nameIs :: Name -> FSItem -> Bool
nameIs name (File fileName _) = name == fileName
nameIs name (Folder folderName _) = name == folderName

root :: FSItem -> FSZipper
root r = (r, [])

goUp :: FSZipper -> FSZipper
goUp (i, ((FSCrumb parent l r):cs)) = (Folder parent (l ++ [i] ++ r), cs)

goDownTo :: Name -> FSZipper -> FSZipper
goDownTo name (Folder folderName items, cs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs : cs)

rename :: Name -> FSZipper -> FSZipper
rename newName (File _ contents, cs) = (File newName contents, cs)
rename newName (Folder _ files, cs) = (Folder newName files, cs)

create :: FSItem -> FSZipper -> FSZipper
create item (Folder name files, cs) = (Folder name (item:files), cs)

myDisk :: FSItem
myDisk = Folder "root" [
    File "wtf.txt" "wtf",
    File "wtf2.txt" "wtf",
    Folder "pics" [
        File "wtf.jpg" "jpg",
        Folder "test" [
            File "t1.txt" "t1",
            File "t2.txt" "t2"
        ]
    ]]

(-:) :: a -> (a -> b) -> b
(-:) x f = f x
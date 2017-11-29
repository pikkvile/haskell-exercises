import Data.Char

main = do
    line <- getLine
    if (null line)
        then putStrLn "Bad input"
        else putStrLn (upp line)

upp :: String -> String
upp "" = ""
upp (x:xs) = toUpper x : upp xs
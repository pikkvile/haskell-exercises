main = do
    concatenated <- (++) <$> getLine <*> getLine
    putStrLn concatenated
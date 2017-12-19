main = do
    reversed <- fmap reverse getLine
    putStrLn reversed
main = interact eval

eval :: String -> String
eval = show . head . foldl ff [] . words
    where
        ff :: [Double] -> String -> [Double]
        ff (n2:n1:ts) "+" = (n1 + n2) : ts
        ff (n2:n1:ts) "-" = (n1 - n2) : ts
        ff (n2:n1:ts) "*" = (n1 * n2) : ts
        ff (n2:n1:ts) "/" = (n1 / n2) : ts
        ff acc num = (read num) : acc
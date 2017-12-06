main = interact arePals

arePals :: String -> String
arePals c = unlines (map (\w -> w ++ " " ++ (isPal w)) (lines c))

isPal :: String -> String
isPal s = if (s == reverse s) then "yep" else "nope"
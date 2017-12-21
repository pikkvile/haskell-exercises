main = interact isPal

isPal :: String -> String
isPal = unlines . map (\s -> show (s == reverse s)) . lines
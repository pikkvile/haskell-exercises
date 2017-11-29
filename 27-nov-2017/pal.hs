import Control.Monad

main = interact testPalindromes

testPalindromes :: String -> String
testPalindromes = unlines . map (\l -> if isPal l then "YES" else "NO") . lines
    where
        isPal :: String -> Bool
        isPal l = l == reverse l
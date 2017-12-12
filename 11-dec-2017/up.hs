import Data.Char

main = interact up

up :: String -> String
up = map Data.Char.toUpper
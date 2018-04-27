module SafeRPN where

import Control.Monad (liftM, foldM)

solveRPN :: String -> Maybe Double
solveRPN st = do
    stack <- foldM rpn [] (words st)
    return (head stack)

rpn :: [Double] -> String -> Maybe [Double]
rpn (a:b:rest) "+" = Just $ (a + b) : rest
rpn (b:a:rest) "-" = Just $ (a - b) : rest
rpn (a:b:rest) "*" = Just $ (a * b) : rest
rpn (b:a:rest) "/" = Just $ (a / b) : rest
rpn stack num = liftM (:stack) (readMaybe num)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
    [(x, "")] -> Just x
    _ -> Nothing

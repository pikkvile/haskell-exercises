module RPN2 where

import Control.Monad (foldM, liftM)

eval :: String -> Maybe Double
eval expr = liftM head (foldM rpn [] . words $ expr)

readM :: (Read a) => String -> Maybe a
readM s = case reads s of
    [(x, [])] -> Just x
    _ -> Nothing

rpn :: [Double] -> String -> Maybe [Double]
rpn (a:b:tail) "+" = Just $ (a + b) : tail
rpn (b:a:tail) "-" = Just $ (a - b) : tail
rpn (a:b:tail) "*" = Just $ (a * b) : tail
rpn (b:a:tail) "/" = Just $ (a / b) : tail
rpn stack num = liftM (:stack) (readM num)
module RPN where

import Control.Monad (foldM, liftM)

type Stack = [Double]

eval :: String -> Maybe Double
eval = fmap head . foldM rpn [] . words

readM :: String -> Maybe Double
readM num = case reads num of
    (d:[]) -> Just (fst d)
    _ -> Nothing

rpn :: Stack -> String -> Maybe Stack
rpn (a:b:stack) "+" = Just $ (a + b) : stack
rpn (b:a:stack) "-" = Just $ (a - b) : stack
rpn (a:b:stack) "*" = Just $ (a * b) : stack
rpn (b:a:stack) "/" = Just $ (a / b) : stack
rpn stack num = liftM (:stack) $ readM num
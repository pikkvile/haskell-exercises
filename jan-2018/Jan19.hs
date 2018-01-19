module Jan19 where

type Pole = (Int, Int)

landLeft :: Int -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs (left + n - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Int -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - right - n) < 4 = Just (left, right + n)
    | otherwise = Nothing

procedure :: Maybe Pole
procedure = do
    initial <- return (0, 0)
    first <- landLeft 3 initial
    second <- landRight 1 first
    third <- landLeft 1 second
    landRight 1 third
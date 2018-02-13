module Feb2 where

ffmap :: (a -> b) -> (r -> a) -> (r -> b)
ffmap f g = (\x -> f (g x))


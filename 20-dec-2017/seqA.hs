import Control.Applicative

sequenceAr :: (Applicative f) => [f a] -> f [a]
sequenceAr [] = pure []
sequenceAr (x:xs) = (:) <$> x <*> sequenceAr xs

sequenceAf :: (Applicative f) => [f a] -> f [a]
sequenceAf = foldr (liftA2 (:)) (pure [])
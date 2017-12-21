import Control.Applicative

lA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lA2 f a1 a2 = f <$> a1 <*> a2
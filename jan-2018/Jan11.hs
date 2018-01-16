module Jan11 where

import Control.Applicative

z3 :: (a -> b -> c -> d) -> ZipList a -> ZipList b -> ZipList c -> ZipList d
z3 f xs ys zs = f <$> xs <*> ys <*> zs
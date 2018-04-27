module Tree3 where

data T a = E | N a (T a) (T a) deriving Show

testTree = N 1 (N 2 E E) (N 3 E E)

data C a = L a (T a) | R a (T a) deriving Show
type Z a = (T a, [C a])

goLeft :: Z a -> Maybe (Z a)
goLeft (N v l r, zs) = case l of
    E -> Nothing
    t -> Just (l, (L v r) : zs)

goUp :: Z a -> Maybe (Z a)
goUp (t, []) = Nothing
goUp (t, (L v r:zs)) = Just (N v t r, zs)
goUp (t, (R v l:zs)) = Just (N v l t, zs)

goRight :: Z a -> Maybe (Z a)
goRight (N v l r, zs) = case r of
    E -> Nothing
    t -> Just (r, (R v l) : zs)
import Control.Applicative

zw3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zw3 f l1 l2 l3 = getZipList $ f <$> (ZipList l1) <*> (ZipList l2) <*> (ZipList l3)
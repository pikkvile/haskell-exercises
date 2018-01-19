module Jan17 where

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
apllyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x
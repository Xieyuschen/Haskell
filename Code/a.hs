instance Functor Maybe where
    fmap f (Just x) =Just (f x)
    fmap f Nothing = Nothing
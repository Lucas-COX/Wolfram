module Utils where

maybeSub :: Maybe Int -> Maybe Int -> Maybe Int
maybeSub (Just x) (Just y) = Just (x - y)
maybeSub Nothing _ = Nothing
maybeSub _ Nothing = Nothing

reverseList :: [x] -> [x]
reverseList = foldl (flip(:)) []

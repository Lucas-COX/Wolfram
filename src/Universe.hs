module Universe where

data U x = U [x] x [x] deriving Show

instance Functor U where
    fmap f (U ls x rs) = U (map f ls) (f x) (map f rs)

concatStrings :: [String] -> String
concatStrings (a:as) = foldl (++) a as
concatStrings [] = ""

uToString :: Show x => U x -> String
uToString u = concatStrings $ map show $ uToList u

lshift :: U x -> Maybe (U x)
lshift (U ls x (r: rs)) = Just (U (x:ls) r rs)
lshift _ = Nothing

rshift :: U x -> Maybe (U x)
rshift (U (l:ls) x rs) = Just (U ls l (x:rs))
rshift u = Nothing

shift :: U x -> Int -> Maybe (U x)
shift u n = if n < 0
    then case lshift u of
        Just uni -> shift uni (n + 1)
        Nothing -> Nothing
    else case rshift u of
        Just uni -> shift uni (n - 1)
        Nothing -> Nothing

llist :: U x -> [x]
llist (U ls _ _) = ls

rlist :: U x -> [x]
rlist (U _ _ rs) = rs

uToList :: U x -> [x]
uToList (U ls x rs) = ls ++ [x] ++ rs

getNeighborhood :: U x -> [x]
getNeighborhood (U (l:ls) x (r:rs)) = [l, x, r]
getNeighborhood U {} = []

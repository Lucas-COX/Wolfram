module Universe (
    U (..),
    uToString,
    getNeighborhood,
    getNCells
) where

import Utils ( reverseList )

data U x = U [x] x [x] deriving Show

instance Functor U where
    fmap f (U ls x rs) = U (map f ls) (f x) (map f rs)

concatStrings :: [String] -> String
concatStrings (a:as) = foldl (++) a as
concatStrings [] = ""

uToString :: Show x => U x -> String
uToString (U ls x rs) =
    concatStrings (map show $ reverseList ls)
    ++ show x ++ concatStrings (map show rs)

lshift :: U x -> U x
lshift (U ls x (r: rs)) = U (x:ls) r rs
lshift u = u

rshift :: U x -> U x
rshift (U (l:ls) x rs) = U ls l (x:rs)
rshift u = u

shift :: U x -> Int -> U x
shift u n = if n < 0
    then shift (lshift u) (n + 1)
    else shift (rshift u) (n - 1)

llist :: U x -> [x]
llist (U ls _ _) = ls

rlist :: U x -> [x]
rlist (U _ _ rs) = rs

uToList :: U x -> [x]
uToList (U ls x rs) = ls ++ [x] ++ rs

getNeighborhood :: U x -> [x]
getNeighborhood (U (l:ls) x (r:rs)) = [l, x, r]
getNeighborhood U {} = []

getNCells :: Int -> U x -> U x
getNCells n (U ls x rs)
    | n <= 0 = U ls x rs
    | even n = U (take (n `div` 2) ls) x (take (n `div` 2 - 1) rs)
    | odd n = U (take (n `div` 2) ls) x (take (n `div` 2) rs)
getNCells _ (U _ x _) = U [] x []

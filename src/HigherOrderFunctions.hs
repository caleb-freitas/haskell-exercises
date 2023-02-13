module HigherOrderFunctions
    (
    ,
    ) where

import Prelude hiding (takeWhile, dropWhile)

{- | Exercise 1 -}
xpto :: (a -> b) -> (a -> Bool) -> [a] -> [b]
xpto f p = map f . filter p

{- | Exercise 2 -}
all :: (a -> Bool) -> [a] -> Bool
all p xs = and $ map p xs

any :: (a -> Bool) -> [a] -> Bool
any p xs = or $ map p xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
    | p x = x : takeWhile p xs
    | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
    | p x = dropWhile p xs
    | otherwise = x : xs

{- | Exercise 3 -}
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x xs -> if p x then x:xs else xs) []

{- | Exercise 4 -}
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

{- | Exercise 5 -}


{- | Exercise 6 -}


{- | Exercise 7 -}


{- | Exercise 8 -}


{- | Exercise 9 -}


{- | Exercise 10 -}


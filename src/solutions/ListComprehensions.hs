module ListComprehensions
    ( squaredSum
    , grid
    , square
    , replicate
    , pyths
    , factors
    , perfect
    , perfects
    , positionsZip
    , find
    , scalarProduct
    ) where

import Prelude hiding (replicate)

{- | Exercise 1 -}
squaredSum :: Int -> Int
squaredSum k = sum [n^2 | n <- [1..k]]

{- | Exercise 2 -}
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

{- | Exercise 3 -}
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

{- | Exercise 4 -}
replicate :: Int -> a -> [a]
replicate n k = [k | _ <- [0..n - 1]]

{- | Exercise 5 -}
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

{- | Exercise 6 -}
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfect :: Int -> Bool
perfect n = sum (factors n) == n

perfects :: Int -> [Int]
perfects n = [k | k <- [1..n], perfect k]

{- | Exercise 7

Prelude> concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]
-}

{- | Exercise 8 -}
-- using zip
positionsZip :: Eq a => a -> [a] -> [Int]
positionsZip x xs = [r | (q, r) <- zip xs [0..], x == q]

-- using find
find :: Eq a => a -> [(a,b)] -> [b]
find k table = [r | (q, r) <- table, k == q]

-- positionsFind :: Eq a => a -> [a] -> [Int]
-- TODO

{- | Exercise 9 -}
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [r * s | (r, s) <- zip xs ys]

{- | Exercise 10 -}
-- TODO

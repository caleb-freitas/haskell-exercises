module RecursiveFunctions
    ( factorial
    , sumdown
    , (^)
    , euclid
    , length
    , drop
    , init
    , and
    , replicate
    , (!!)
    , elem
    , merge
    , msort
    , sum
    , take
    , last
    ) where

import Prelude hiding ((^), length, sum, last, take, drop, init, and, concat, (!!), elem, replicate)

{- | Exercise 1 -}
factorial :: Int -> Int
factorial n | n < 0 = error "invalid argument"
            | n == 0 = 1
            | otherwise = n * factorial(n - 1)

{- | Exercise 2 -}
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown(n - 1)

{- | Exercise 3 -}
(^) :: Int -> Int -> Int
m ^ 1 = m
m ^ n = m * (m ^ (n - 1))

-- Evaluation:
-- 3 ^ 3
-- 3 * (3 ^ 2)
-- 3 * (3 * (3 ^ 1))
-- 3 * (3 * 3)
-- 27

{- | Exercise 4 -}
euclid :: Int -> Int -> Int
euclid m n | m <= 0 || n <= 0 = error "invalid argument"
           | m == n = m
           | m < n = euclid m (n - m)
           | otherwise = euclid (m - n) n

{- | Exercise 5 -}
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

-- Evaluation:
-- length [1,2,3]
-- 1 + (length [2, 3])
-- 1 + (1 + (length [3]))
-- 1 + (1 + (1 + length[]))
-- 1 + (1 + 1 (0)
-- 3

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

-- Evaluation:
-- drop 3 [1,2,3,4,5]
-- drop 2 [2,3,4,5]
-- drop 1 [3,4,5]
-- drop 0 [4,5]
-- [4,5]
     
init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs

-- Evaluation:
-- init [1,2,3]
-- 1 : (init [2,3])
-- 1 : (2 : (init [3]))
-- 1: (2 : [])
-- [1,2]

{- | Exercise 6 -}
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

-- Evaluation:
-- and [True, True, False]
-- True && (and [True, False])
-- True && (True && (and [False]))
-- True && (True && (False && and []))
-- True && (True && (False && True))
-- True && (True && False)
-- True && False
-- False

-- concat :: [[a]] -> [a]
-- concat [[],[]] = []
-- concat [[a],[]] = [a]
-- concat [[],[a]] = [a]
-- concat xs (y:ys) = error "to implement"

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n k = k : replicate (n - 1) k

-- Evaluation:
-- replicate 3 "x"
-- "x" : (replicate 2 "x")
-- "x" : ("x" : (replicate 1 "x"))
-- "x" : ("x" : ("x" : (replicate 0 "x")))
-- "x" : ("x" : ("x" : ([])))
-- ["x", "x", "x"]

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

-- Evaluation:
-- [1,2,3,4,5] !! 2
-- [2,3,4,5] !! 1
-- [3,4,5] !! 0
-- 3

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs) = (e == x) || elem e xs

-- Evaluation:
-- elem 3 [1,2,3,4,5]
-- elem 3 [2,3,4,5]
-- elem 3 [3,4,5]
-- True

-- elem 10 [1,2,3]
-- elem 10 [2,3]
-- elem 10 [3]
-- False

{- | Exercise 7 and 8 -}
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y =  x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where (ys, zs) = splitAt (length xs `div` 2) xs

-- Evaluation:
-- msort [4,2,3,1]
-- merge (msort [4,2]) (msort [3,1])
-- merge (merge (msort [4]) (msort [2])) (merge (msort [3]) (msort [1]))
-- merge (merge [4] [2]) (merge [3] [1])
-- merge (2 : merge [4] []) (1 : merge [3] [])
-- merge (2 : [4]) (1 : [3])
-- merge [2,4] [1,3]
-- 1 : (merge [2,4] [3])
-- 1 : (2 : (merge [4] [3]))
-- 1 : (2 : (3 : merge [4] []))
-- 1 : (2 : (3 : [4]))
-- [1,2,3,4]

{- | Exercise 9 -}
sum :: [Int] -> Int
sum [] = 0
sum (n:ns) = n + sum ns

take :: Int -> [a] -> [a]
take _ [] = []
take 0 xs = []
take n (x:xs) = x : take (n-1) xs

last :: [a] -> a
last [x] = x
last (x:xs) = last xs

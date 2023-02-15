module VotingAlgorithms
    (
    ,
    ) where

import Data.List

-- First past the post algorithm

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- Counts the number of times that a given value occurs in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- Removes duplicate values from a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- Returns the result of a first-past-the-post election in increasing order of the
-- number of votes received
result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Alternative vote

-- Remove empty ballots and eliminate a given candidate from each ballot
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a ->  [[a]] -> [[a]]
elim x = map $ filter (/= x)

-- Ranks the 1st-choice candidates in each ballot in increasing order of the
-- number of such votes that were received
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

altWinner :: Ord a => [[a]] -> a
altWinner bs = case rank (rmempty bs) of
                   [c] -> c
                   (c:cs) -> altWinner (elim c bs)

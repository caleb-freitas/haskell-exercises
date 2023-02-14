module SudokuSolver () where

import Data.List (transpose)

-- | Basic declarations
type Value = Char
type Row a = [a]
type Matrix a = [Row a]
type Grid = Matrix Value

-- | Basic definitions
boxsize :: Int
boxsize = 3

values :: [Value]
values = ['1'..'9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

-- | Extract rows from a matrix.
rows :: Matrix a -> [Row a]
rows = id

-- | Extract columns from a matrix.
cols :: Matrix a -> [Row a]
cols = transpose

-- | Extract boxes from a matrix.
boxes:: Matrix a -> [Row a]
boxes = unpack . map cols . pack
    where
        pack = split . map split
        split = chop boxsize
        unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

valid :: Grid -> Bool
valid g =
    all nodups (rows g)
        && all nodups (cols g)
        && all nodups (boxes g)

nodups :: Eq a => [a] -> Bool
nodups [] = True 
nodups (x:xs) = notElem x xs && nodups xs

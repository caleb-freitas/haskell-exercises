module SudokuSolver 
    ( bruteForceSolver
    , pruneSolver
    , iterativeSolver
    ) where

import Data.List (transpose, (\\))

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

-- | Examples grids
easy :: Grid
easy =
    [
        "2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..8...2.",
        ".5..69784",
        "4..25...."
    ]

gentle :: Grid
gentle =
    [
        ".1.42...5",
        "..2.71.39",
        ".......4.",
        "2.71....6",
        "....4....",
        "6....74.3",
        ".7.......",
        "12.73.5..",
        "3...82.7."
    ]

diabolical :: Grid
diabolical =
    [
        ".9.7..86.",
        ".31..5.2.",
        "8.6......",
        "..7.5...6",
        "...3.7...",
        "5...1.7..",
        "......1.9",
        ".2.6..35.",
        ".54..8.7."
    ]

unsolvable :: Grid
unsolvable =
    [
        "1..9.7..3",
        ".8.....7.",
        "..9...6..",
        "..72.94..",
        "41.....95",
        "..85.43..",
        "..3...7..",
        ".5.....4.",
        "2..8.6..9"
    ]

minimal :: Grid
minimal =
    [
        ".98......",
        "....7....",
        "....15...",
        "1........",
        "...2....9",
        "...9.6.82",
        ".......3.",
        "5.1......",
        "...4...2."
    ]

blank :: Grid
blank = replicate n (replicate n '.')
    where
        n = boxsize ^ 2

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

-- | Checks for a valid grid (following the three basic rules of sudoku).
valid :: Grid -> Bool
valid g =
    all nodups (rows g)
        && all nodups (cols g)
        && all nodups (boxes g)

-- | Checks for duplicated values in a list.
nodups :: Eq a => [a] -> Bool
nodups [] = True 
nodups (x:xs) = notElem x xs && nodups xs

-- | Basic solver using brute force (takes too long).
bruteForceSolver :: Grid -> [Grid]
bruteForceSolver = filter valid . explode . choices

-- | Replaces each blank cell by all of possible number between 1 and 9.
type Choices = [Value]
choices :: Grid -> Matrix Choices
choices = map (map choice)
    where
        choice v = if v == '.' then ['1'..'9'] else [v]

-- | Explode a matrix of choices to a choice of matrices by reducing all possible
-- combinations of choices for each cell.
explode :: Matrix [a] -> [Matrix a]
explode m = cartesianProduct (map cartesianProduct m)

-- | Calculate all the possible combinations of a list of lists values.
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs:xss) = [k:ks | k <- xs, ks <- cartesianProduct xss]

-- | Prunes the search space for invalid choices.
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy cols . pruneBy rows
    where
        pruneBy f = f . map reduce . f

-- | Eliminate choices which have no possibility to ever suceed.
reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss] 
    where
        singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

-- | Improved solver using the prune helper function (still takes too long).
pruneSolver :: Grid -> [Grid]
pruneSolver = filter valid . explode . prune . choices

-- | A better solver using iterative function fix to get the fixed pointe in the
-- list of choices.
iterativeSolver :: Grid -> [Grid]
iterativeSolver = filter valid . explode . fix prune . choices

-- | Get the fixed-point of a list iteractively.
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
    where
        x' = f x

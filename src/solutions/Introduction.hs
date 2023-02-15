module Introduction 
      ( someFunc
      , product
      , reversedQuicksort
      ) where

import Prelude hiding (product)
import GHC.Generics (Generic(from))
import GHC.Exts (the)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- | Exercise 1 -}

{-
double (double 2) =
= double 2 + double 2 =
= (2 + 2) + (2 + 2) =
= 8
-}

{- | Exercise 2 -}

{-
sum [x] =
= x + sum [] =
= x + 0 =
= x
-}

{- | Exercise 3 -}
product :: (Num a) => [a] -> a
product [] = 1
product (x:xs) = x * product xs
-- Alternative to recursion using `foldr`:
-- product xs = foldr (*) 1 xs

{-
product [2, 3, 4] =
= 2 * (product[3, 4])
= 2 * (3 * product[4])
= 2 * (3 * (4 * product[]))
= 2 * (3 * (4 * 1))
= 24
-}

{- | Exercise 4 -}
reversedQuicksort :: (Ord a) => [a] -> [a]
reversedQuicksort[] = []
reversedQuicksort(x:xs) = reversedQuicksort larger ++ [x] ++ reversedQuicksort smaller
                    where
                      larger = [b | b <- xs, b > x]
                      smaller = [a | a <- xs, a <= x]

{- | Exercise 5

Some numbers may be ignored, since the interval a < x < b it's not completely 
defined. This only works in the case where all the numbers are distinct from
each other. In mathematical notation, we can say that this only works for the
interval: {a1, a2, ..., an | ai != aj for all i != j}.
-}

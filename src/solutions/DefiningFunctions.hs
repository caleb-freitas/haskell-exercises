module DefiningFunction
    ( halve
    , thirdHeadTail
    , thirdListIndexing
    , thirdPatternMatching
    , safetailConditional
    , safetailGuarded
    , safetailPatternMatching
    , (&&)
    , mult
    ) where

import Prelude hiding ((&&))

{- | Exercise 1 -}
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
            where n = length xs `div` 2


{- | Exercise 2 -}
thirdHeadTail :: [a] -> a
thirdHeadTail xs = head(tail(tail xs))

thirdListIndexing :: [a] -> a
thirdListIndexing xs = xs !! 2
     
thirdPatternMatching :: [a] -> a
thirdPatternMatching (_ : (_ : (k : _))) = k

{- | Exercise 3 -}
safetailConditional :: [a] -> [a]
safetailConditional xs = if null xs then [] else tail xs

safetailGuarded :: [a] -> [a]
safetailGuarded xs | null xs = []
                   | otherwise = tail xs

safetailPatternMatching :: [a] -> [a]
safetailPatternMatching [] = []
safetailPatternMatching (_:xs) = xs

{- | Exercise 4

-- truth table
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

-- wildcard pattern
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

-- pattern matching
(||) :: Bool -> Bool -> Bool
True || _ = True
False || b = b

-- guarded equations
(||) :: Bool -> Bool -> Bool
p || q | p == True = True
       | q == True = True
       | otherwise = False
-}

{- | Exercise 5 -}
(&&) :: Bool -> (Bool -> Bool)
p && q = if p == True then
           if q == True then True
           else False
          else False

{- | Exercise 6 

(&&) :: Bool -> (Bool -> Bool)
p && q = if p == True then q
         else False
-}

{- | Exercise 7 -}
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

{- | Exercise 8 -}
-- TODO

module FistSteps
    ( myLast
    , myInitTail
    , myInitDrop
    ) where

{- | Exercise 1 

This exercise does not require a written answer *per se*.
-}

{- | Exercise 2 

(2^3)*4
(2*3)+(4*5)
2+(3*(4^5))
-}

{- | Exercise 3 -}
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]
-- n = 10 / 5 = 2.

{- | Exercise 4 -}
myLast :: [a] -> a
myLast ns = head (reverse ns)

{- | Exercise 5 -}
-- first way
myInitTail :: [a] -> [a]
myInitTail ns = reverse (tail (reverse ns))

-- second way
myInitDrop :: [a] -> [a]
myInitDrop ns = reverse (drop 1 (reverse ns))

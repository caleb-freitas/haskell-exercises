module TypesAndClasses
    ( add
    , copy
    , apply
    , second
    , swap
    , pair
    , double
    , palindrome
    , twice
    ) where


{- | Exercise 1 

[Char]
(Char)
[(Bool, Char)]
([Bool], [Char])
[[a] -> [a]]
-}

{- | Exercise 2 -}
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1..10], [2..20]]

add :: Int -> Int -> Int -> Int
add a b c = a ^ 2 + b ^ 2 + c ^ 2

copy :: a -> (a,a)
copy s = (s, s)

apply :: (a -> b) -> a -> b
apply f x = f x

{- | Exercise 3 -}
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

{- | Exercise 4

Done in the GCHi.
-}

{- | Exercise 5

Because, in the first case, to check if two function are equal, videre licet,
they always return equal results to equal arguments, all the arguments must be
enumerated in order to check the output of each value. In this way, it is
unfeasible to check function's equality.
-}

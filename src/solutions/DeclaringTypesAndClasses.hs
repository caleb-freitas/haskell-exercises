module DeclaringTypesAndClasses () where

-- | Exercise 1
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

{-
Multiplication of 2 * 1:

ghci> mult (Succ (Succ Zero)) (Succ Zero)

Evaluation:

add (Succ (Succ Zero)) (mult (Succ (Succ Zero))  Zero)
add (Succ (Succ Zero)) (Zero)
Succ (Succ Zero)
-}


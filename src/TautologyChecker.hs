module TautologyChecker (isTautology) where

-- | Type for propositions, with one constuctor for each of the five possible
-- forms that a proposition can have.
data Proposition
  = Const Bool
  | Var Char
  | Not Proposition
  | And Proposition Proposition
  | Imply Proposition Proposition

-- | Define propositions. 
p1 :: Proposition
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Proposition
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Proposition
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Proposition
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- | Alias for a list of key-value pairs where the keys have type k and the
-- values have type v.
type Assoc k v = [(k, v)]

-- | Declare `Substitution` as a lookup table that associates variable names
-- to logical values, using the `Assoc` type.
type Substitution = Assoc Char Bool

find :: (Eq k) => k -> Assoc k v -> Maybe v
find _ [] = Nothing
find key ((k, v) : rest)
    | key == k = Just v
    | otherwise = find key rest

eval :: Substitution -> Proposition -> Bool
eval _ (Const b) = b
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Var x) = case find x s of
    Just b -> b
    Nothing -> error ("Undefined variable: " ++ [x])

-- | Returns a list of all the variables in a proposition
vars :: Proposition -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- | Return all possible logical values
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
    where
        bss = bools (n - 1)

-- | Removes duplicate values from a list
rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

-- | Generates all posible substitutions for a proposition by extracting its
-- variables, removing duplicates from this list, generating all possible lists
-- of logical values for this many variables, and the zipping the list of variables
-- with each of the resulting list.
substitutions :: Proposition -> [Substitution]
substitutions p = map (zip vs) (bools (length vs))
    where
        vs = rmdups (vars p)

-- | Decides if a proposition is a tautology, by simply checking if it evaluates
-- to True for all possible substitutions.
isTautology :: Proposition -> Bool
isTautology p = and [eval s p | s <- substitutions p]

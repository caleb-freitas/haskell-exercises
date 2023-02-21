module MonadicParsing () where

import Data.Char
import Control.Applicative

-- | A parser that consumes a string and returns a list of parsed values and
-- remaining unparsed input strings.
newtype Parser a = Parser (String -> [(a, String)])

-- | Runs a Parser on a given input string and returns the list of results.
--
-- Example:
--
-- >>> parse charP ""
-- []
--
-- >>> parse charP "abc"
-- [('a', "bc")]
parse :: Parser a -> String -> [(a, String)]
parse (Parser parser) input = parser input

charP :: Parser Char
charP  = Parser (\input -> case input of
    [] -> []
    (x:xs) -> [(x, xs)])

-- | Applies a function to the result value of a parser if the parser succeeds,
-- and propagates the failure otherwise.
--
-- Example:
--
-- >>> parse (fmap toUpper charP) "abc"
-- [('A', "bc")]
--
-- >>> parse (fmap toUpper charP) ""
-- []
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser (\input -> case parse p input of
        [] -> []
        [(parsed, remainder)] -> [(f parsed, remainder)])

-- | Defines Applicative instance for Parser type to apply parsed functions to
-- parsed values.
--
-- Example:
--
-- >>> parse (pure 3) "abc"
-- [(3, "abc")]
instance Applicative Parser where
    -- pure :: a -> Parser a
    pure value = Parser (\input -> [(value, input)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    parser <*> parserValue = Parser (\input -> case parse parser input of
        [] -> []
        [(parsed, remainder)] -> parse (fmap parsed parserValue) remainder)

-- | Defines Monad instance for Parser type to chain parsers together with bind operator (>>=).
--
-- Because Parser is a monadic type, the `do` notation can now be used to sequence
-- parsers and process their result values. For example:
--
-- three :: Parser (Char, Char)
-- three = do
--    x <- charP
--    charP
--    z <- charP
--  return (x, z)
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    parser >>= f = Parser (\input -> case parse parser input of
        [] -> []
        [(parsed, remainder)] -> parse (f parsed) remainder)

-- | Defines Alternative instance for Parser type to provide backtracking and
-- choice when parsing.
--
-- Example:
--
-- >>> parse (char 'a' <|> char 'b') "a"
-- [('a', "")]
--
-- >>> parse (char 'a' <|> char 'b') "b"
-- [('b', "")]
instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser (\input -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = Parser (\input -> case parse p input of
        [] -> parse q input
        [(parsed, remainder)] -> [(parsed, remainder)])

-- | A parser that succeeds if the next character in the input string satisfies
-- a given predicate, and fails otherwise
satisfyP :: (Char -> Bool) -> Parser Char
satisfyP predicate = do
    x <- charP
    if predicate x then return x else empty

-- | Partial parsers to be used with the `token` function.

digit :: Parser Char
digit = satisfyP isDigit

lower :: Parser Char
lower = satisfyP isLower

upper :: Parser Char
upper = satisfyP isUpper

letter :: Parser Char
letter = satisfyP isAlpha

alphanum :: Parser Char
alphanum = satisfyP isAlphaNum

char :: Char -> Parser Char
char c = satisfyP (== c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x:xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
    many (satisfyP isSpace)
    return ()

int :: Parser Int
int = do
    char '-'
    n <- nat
    return (-n)
    <|> nat

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

naturals :: Parser [Int]
naturals = do
    symbol "["
    n <- natural
    ns <- many (do
        symbol ","
        natural)
    symbol "]"
    return (n:ns)

-- | Arithmetic expressions.

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> natural

eval :: String -> Int
eval xs = case parse expr xs of
             [(n,[])]  -> n
             [(_,out)] -> error ("Unused input " ++ out)
             []        -> error "Invalid input"

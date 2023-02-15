module Hangman (hangman) where

import System.IO (hSetEcho, stdin)

hangman :: IO ()
hangman = do
    putStr "Think of a word:" 
    word <- sgetLine
    putStrLn "Try to guess it:"
    play word

-- | Keep word to be guessed in secret replacing characters with `-`.
sgetLine :: IO String
sgetLine = do
    x <- getCh
    if x == '\n'
    then do
        putChar x
        return []
    else do
        putChar '-'
        xs <- sgetLine
        return (x : xs)

-- | Reads a single character from the keyboard without echoing it to the screen.
getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

-- | Repeatedly prompt to the second player to enter a guess until it equals to
-- the secret word.
play :: String -> IO ()
play word = do
    putStr "? "
    guess <- getLine
    if guess == word then
        putStr "You got it!"
    else do
        putStrLn (match word guess)
        play word

-- | Indicate which letters in the secret word occur anywhere in the guess.
match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

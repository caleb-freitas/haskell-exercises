module InteractiveProgramming () where

import System.IO

-- | Exercise 1
showStr :: String -> IO ()
showStr xs = sequence_ [putChar x | x <- xs]

-- | Exercise 2
putRow :: Int -> Int -> IO ()
putRow row num = do
    putStr (show row)
    putStr ": "
    putStr (concat $ replicate num "* ")
    putStr "\n"

putBoard :: Int -> [Int] -> IO ()
putBoard row [] = return ()
putBoard row (n:ns) = do
    putRow row n
    putBoard (row - 1) ns

-- | Exercise 3
putBoardSeq :: [Int] -> IO ()
putBoardSeq board = sequence_ [putRow row num | (row, num) <- zip (reverse [1..n]) board]
    where
        n = length board

-- | Exercise 4
adder :: IO ()
adder = do
    putStr "How many numbers? "
    n <- readLn :: IO Int
    nums <- sequence $ replicate n readLn
    putStrLn $ "The total is " ++ show (sum nums)

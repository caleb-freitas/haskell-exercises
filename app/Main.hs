module Main where

import qualified Introduction (someFunc)
import qualified Introduction as MyLib

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc

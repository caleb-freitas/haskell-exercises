module BinaryStringTransmitter
    ( binToInt
    , intToBin
    , fillEightBits
    , chopEightBits
    , encode
    , decode
    , transmit
    , channel
    ) where

import Data.Char
import Foreign.C (eNETUNREACH)

-- Make `Int` type more meaningful
type Bit = Int

-- Base conversion

-- Convert a binary number represented as a list of bits in a integer number
binToInt :: [Bit]  -> Int
binToInt []     = 0
binToInt (x:xs) = x + 2 * binToInt xs

-- ghci> binToInt [1,0,1,1]
-- 13

-- Convert a integer number in a binary number represented as a list of bits
intToBin :: Int -> [Bit]
intToBin n = n `mod` 2 : intToBin (n `div` 2)

-- ghci> intToBin 13
-- [1,0,1,1]

-- Ensure that all binary number have the same length of eight bits
fillEightBits :: [Bit] -> [Bit]
fillEightBits bits = take 8 (bits ++ repeat 0)

-- ghci> fillEightBits [1,0,1,1]
-- [1,0,1,1,0,0,0,0]

-- Transmission

-- Encodes a string of characters as a list of bits by converting each
-- character into a Unicode number, converting each such number into an eight-bit
-- binary number, anc concatenating each of these numbers together to produce
-- a list of bits.
encode :: String -> [Bit]
encode = concatMap (fillEightBits . intToBin . ord)

-- ghci> encode "abc"
-- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

-- Chops a list of bits up into a eight-bit binary numbers
chopEightBits :: [Bit] -> [[Bit]]
chopEightBits [] = []
chopEightBits bits = take 8 bits : chopEightBits (drop 8 bits)

-- ghci> chopEightBits [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
-- [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]

-- Decodes a list of bits as a string of characters by chopping the list up,
-- and converting each resulting binary number into a Unicode number and then a
-- character.
decode :: [Bit] -> String
decode = map (chr . binToInt) . chopEightBits

-- ghci> decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
-- "abc"

-- Simulates the transmission of a string of characters as a list of bits, using
-- a perfect communication channel that we model usign the identity function.
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- ghci> transmit "goodbye, world!"
-- "goodbye, world!"

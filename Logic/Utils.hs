{-
Contains a variety of functions designed to test the circuitry 
in different ways and with different data types.
-}

module Logic.Utils where

import Prelude hiding (not, and, or) 
import Logic.Types
import Logic.Circuits (rippleAddN)




--------- ARITHMETIC


-- Performs unsigned binary addition on binary literals using the simulated circuitry.
binAdder :: Int -> Int -> [Bit]
binAdder x y
    | x < 0 || y < 0 = error "Integers must be >= 0"
    | otherwise      = rippleAddN Zero (bin2bit x) (bin2bit y) 


-- Performs addition on non-negative decimal integers using the simulated circuitry.
intAdder :: Int -> Int -> Int
intAdder x y
    | x < 0 || y < 0 = error "Integers must be >= 0"
    | otherwise      = bit2intUnsigned $ rippleAddN Zero (int2bit x) (int2bit y)



-------- TYPE CONVERTERS


-- Takes a non-negative decimal integer and produces a list of Bits.
int2bit :: Int -> [Bit]
int2bit 0 = [Zero]
int2bit n 
    | n < 0     = error "Integer must be >= 0" 
    | otherwise = unfold (== 0) convertBit (`div` 2) n
    where
        convertBit x = if x `mod` 2 == 1 then One else Zero


-- Takes an unsigned binary literal and produces a safe decimal integer.
bin2int :: Int -> Int
bin2int bin = bit2intUnsigned $ bin2bit bin


-- Takes an unsigned binary String and produces a list of Bits (BIG ENDIAN).
binStr2bit :: String -> [Bit]
binStr2bit binStr = reverse $ foldr go [] binStr
    where
        go b recur
            | b == '0' = Zero : recur
            | b == '1' = One : recur
            | otherwise = recur


-- Takes an unsigned list of Bits and produces a string.
bit2string :: [Bit] -> String
bit2string bits = foldr go [] bits
    where
        go b recur
            | b == Zero = '0' : recur
            | b == One  = '1' : recur


-- Takes a binary literal and produces a safe list of Bits.
bin2bit :: Int -> [Bit]
bin2bit bin = 
    let
        binStr      = show bin 
        validBinStr = isValidBinary binStr
    in
        if validBinStr
        then binStr2bit binStr
        else [Zero]


-- Takes an unsigned list of n Bits and produces a decimal integer.
bit2intUnsigned :: [Bit] -> Int
bit2intUnsigned bits = sum $ zipWith (*) values powers
    where
        toVal Zero = 0
        toVal One  = 1
        values     = map toVal bits
        powers     = iterate (*2) 1


-- Takes a signed list of n Bits and produces a decimal integer.
bit2intSigned :: [Bit] -> Int
bit2intSigned bits =
    let
        msb           = last bits
        magnitudeBits = init bits
        magnitudeVal  = bit2intUnsigned magnitudeBits
        signVal       = if msb == One 
                        then -(2 ^ (length bits - 1)) 
                        else 0
    in
        magnitudeVal + signVal




-------- HELPER FUNCTIONS


-- Determines whether a String represents a valid binary number.
isValidBinary :: String -> Bool
isValidBinary binStr = all (\c -> c == '0' || c == '1') binStr


-- Checks the sign bit.
isNegative :: [Bit] -> Bool
isNegative bits
    | last bits == One = True
    | otherwise        = False


-- Uses an initial value and a predicate to generate a list.
unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x 
    | p x = []
    | otherwise = h x : unfold p h t (t x)
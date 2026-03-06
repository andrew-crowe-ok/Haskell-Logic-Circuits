module Logic.Utils where

import Prelude hiding (not, and, or) 
import Logic.Gates
import Logic.Types
import Logic.Circuits (rippleAddN)


bits10 = [One, Zero, One, Zero]
bits5  = reverse $ tail $ reverse bits10


-- Takes a decimal integer and produces clean binary output
int2binIO :: Int -> IO ()
int2binIO n = putStrLn $ reverse $ bit2string $ int2bit n


-- Takes a decimal integer and produces a list of Bits
int2bit :: Int -> [Bit]
int2bit 0 = [Zero]
int2bit n = unfold (== 0) convertBit (`div` 2) n
    where
        convertBit x = if x `mod` 2 == 1 then One else Zero


-- Takes decimal 1's and 0's, interprets them as binary, and produces a safe decimal integer
bin2int :: Int -> Int
bin2int bin = bit2intUnsigned $ bin2bit bin


-- Takes a binary String and produces a list of Bits
binStr2bit :: String -> [Bit]
binStr2bit binStr = reverse $ foldr go [] binStr
    where
        go b recur
            | b == '0' = Zero : recur
            | b == '1' = One : recur
            | otherwise = recur


-- Takes decimal 1's and 0's, interprets them as binary, and produces a safe list of Bits
bin2bit :: Int -> [Bit]
bin2bit bin = 
    let
        binStr = show bin 
        validBinStr = isValidBinary binStr
    in
        if validBinStr
        then binStr2bit binStr
        else [Zero]
    

-- Displays a binary number created by binAdder
binAdderIO :: Int -> Int -> IO ()
binAdderIO x y = int2binIO $ bit2intUnsigned $ binAdder x y


-- Performs binary addition using the simulated circuitry.
binAdder :: Int -> Int -> [Bit]
binAdder x y = rippleAddN Zero (bin2bit x) (bin2bit y) 


-- Performs decimal integer addition using the simulated circuitry
intAdder :: Int -> Int -> Int
intAdder x y = bit2intUnsigned $ rippleAddN Zero (int2bit x) (int2bit y)


-- Determines whether a String represents a valid binary number
isValidBinary :: String -> Bool
isValidBinary binStr = all (\c -> c == '0' || c == '1') binStr


-- Takes a list of Bits and produces a decimal integer
bit2intUnsigned :: [Bit] -> Int
bit2intUnsigned bits = sum $ zipWith (*) values powers
    where
        toVal Zero = 0
        toVal One  = 1
        values     = map toVal bits
        powers     = iterate (*2) 1


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


isNegative :: [Bit] -> Bool
isNegative bits
    | last bits == One = True
    | otherwise        = False


-- Takes a list of Bits and produces a string representation
bit2string :: [Bit] -> String
bit2string bits = foldr go [] bits
    where
        go b recur
            | b == Zero = '0' : recur
            | b == One  = '1' : recur


unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x 
    | p x = []
    | otherwise = h x : unfold p h t (t x)
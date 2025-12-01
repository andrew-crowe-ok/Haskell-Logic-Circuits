module Logic.Byte where

import Logic.Types
import Logic.Circuits (rippleAddN)
import qualified Logic.Gates as G
import Logic.Utils (int2bit, bit2intUnsigned)

width :: Int
width = 8

--------------------------------------------------------------------------------
-- PRIVATE HELPERS
--------------------------------------------------------------------------------

-- Ensures a list is exactly 8 bits (Pad LSBs with Zero if short, truncate if long)
-- Note: In this system, LSB is at the head (index 0).
setLength :: [Bit] -> [Bit]
setLength bits = 
    let len = length bits
    in if len < width
       then bits ++ replicate (width - len) Zero -- Pad end (MSB side in this LSB-first system)
       else take width bits                      -- Truncate

-- Extract raw bits (Unwrap)
getBits :: Byte -> [Bit]
getBits (Byte bits) = bits

--------------------------------------------------------------------------------
-- CONSTRUCTORS (Int <-> Byte)
--------------------------------------------------------------------------------

-- Smart Constructor: Converts Int to Byte (Handles Signed 2's Complement automatically)
int2byteSigned :: Int -> Byte
int2byteSigned n
    | n >= 0    = Byte $ setLength (int2bit n)
    | otherwise = 
        -- 2's Complement Algorithm for negatives:
        -- 1. Get positive bits
        -- 2. Invert bits
        -- 3. Add 1
        let 
            posBits  = setLength (int2bit (abs n))
            inverted = map G.not posBits
            rawPlus1 = rippleAddN Zero inverted [One] -- Add 1 to inverted
        in 
            Byte (setLength rawPlus1)

-- Unsigned Interpretation (Treats raw bits as 0 to 255)
byteToIntUnsigned :: Byte -> Int
byteToIntUnsigned (Byte bits) = bit2intUnsigned bits

-- Signed Interpretation (Treats raw bits as -128 to 127)
byteToIntSigned :: Byte -> Int
byteToIntSigned (Byte bits) = 
    let 
        -- In LSB-first, the last bit is the MSB (Sign Bit)
        msb = last bits
        magnitudeBits = init bits 
        magnitudeVal  = bit2intUnsigned magnitudeBits
        
        -- If MSB is 1, subtract 2^7 (128). If 0, add 0.
        signVal = if msb == One then -128 else 0
    in 
        magnitudeVal + signVal

--------------------------------------------------------------------------------
-- LOGIC OPERATIONS
--------------------------------------------------------------------------------

bitwiseNot :: Byte -> Byte
bitwiseNot (Byte bits) = Byte (map G.not bits)

-- Calculates -x using 2's complement (Invert + 1)
negateByte :: Byte -> Byte
negateByte b = 
    let 
        inverted = bitwiseNot b
        one      = int2byteSigned 1
    in 
        addBytes inverted one

--------------------------------------------------------------------------------
-- ARITHMETIC OPERATIONS
--------------------------------------------------------------------------------

-- Safe Addition: Wraps/Unwraps automatically
addBytes :: Byte -> Byte -> Byte
addBytes (Byte a) (Byte b) = 
    let 
        rawSum = rippleAddN Zero a b
    in 
        Byte (setLength rawSum)

-- Safe Subtraction: a - b = a + (-b)
subBytes :: Byte -> Byte -> Byte
subBytes a b = addBytes a (negateByte b)
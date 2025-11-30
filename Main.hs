module Main where

import System.IO (hFlush, stdout) -- Needed to force print prompts immediately
import Logic.Types
import Logic.Utils
import Logic.Circuits


-- Entry point
main :: IO ()
main = do
    putStrLn "========================================"
    putStrLn "   HASKELL LOGIC CIRCUIT SIMULATOR"
    putStrLn "========================================"
    menuLoop


-- The Recursive Menu Loop
menuLoop :: IO ()
menuLoop = do
    putStrLn "\nSELECT AN OPERATION:"
    putStrLn "1. Convert Decimal to Binary"
    putStrLn "2. Convert Binary to Decimal"
    putStrLn "3. Simulate Addition (Decimal Inputs)"
    putStrLn "4. Simulate Addition (Binary Inputs)"
    putStrLn "5. View Utility Function Reference (Help)"
    putStrLn "6. Exit"
    
    putStr "> "
    hFlush stdout -- Ensures the ">" appears before waiting for input
    
    choice <- getLine
    case choice of
        "1" -> runDecToBin >> menuLoop
        "2" -> runBinToDec >> menuLoop
        "3" -> runDecAdd   >> menuLoop
        "4" -> runBinAdd   >> menuLoop
        "5" -> runHelp     >> menuLoop
        "6" -> putStrLn "Exiting simulation. Goodbye!"
        _   -> putStrLn "Invalid selection. Please try again." >> menuLoop


-------------------------------------------------------------------------
-- MENU ACTIONS
-- These "Wrapper Functions" handle the IO and call your pure logic
-------------------------------------------------------------------------

-- Option 1: Decimal -> Binary
runDecToBin :: IO ()
runDecToBin = do
    putStr "Enter a decimal number (e.g., 42): "
    hFlush stdout
    input <- getLine
    let n = read input :: Int
    
    -- Call Logic
    let bits = int2bit n
    let output = reverse $ bit2string bits
    
    putStrLn $ "Binary Output: " ++ output


-- Option 2: Binary -> Decimal
runBinToDec :: IO ()
runBinToDec = do
    putStr "Enter a binary string (e.g., 101010): "
    hFlush stdout
    input <- getLine
    
    -- Call Logic
    let bits = binStr2bit input
    let n = bit2int bits
    
    putStrLn $ "Decimal Output: " ++ show n


-- Option 3: Addition (Input: Dec, Output: Dec)
runDecAdd :: IO ()
runDecAdd = do
    putStr "Enter first number: "
    hFlush stdout
    inputA <- getLine
    
    putStr "Enter second number: "
    hFlush stdout
    inputB <- getLine
    
    let a = read inputA :: Int
    let b = read inputB :: Int
    
    -- Simulation Logic
    let bitsA = int2bit a
    let bitsB = int2bit b
    let resultBits = rippleAddN Zero bitsA bitsB
    let resultInt = bit2int resultBits
    
    putStrLn $ "--- Simulation Result ---"
    putStrLn $ show a ++ " + " ++ show b ++ " = " ++ show resultInt


-- Option 4: Addition (Input: Bin, Output: Bin)
runBinAdd :: IO ()
runBinAdd = do
    putStr "Enter first binary string: "
    hFlush stdout
    inputA <- getLine
    
    putStr "Enter second binary string: "
    hFlush stdout
    inputB <- getLine
    
    -- Simulation Logic
    let bitsA = binStr2bit inputA
    let bitsB = binStr2bit inputB
    let resultBits = rippleAddN Zero bitsA bitsB
    let resultStr = reverse $ bit2string resultBits
    
    putStrLn $ "--- Simulation Result ---"
    putStrLn $ inputA ++ " + " ++ inputB ++ " = " ++ resultStr
    
    -- Option 5: HELP / Reference Guide
runHelp :: IO ()
runHelp = do
    putStrLn "\n=== UTILITY FUNCTION REFERENCE ==="
    putStrLn "These functions are available in Logic.Utils for use in GHCi."
    putStrLn "Note: 'Bit List' implies LSB-First order (e.g., [1, 2, 4...])."
    putStrLn "------------------------------------------------------------"

    putStrLn "1. int2binIO :: Int -> IO ()"
    putStrLn "   Takes a decimal integer (e.g., 5) and prints its binary representation."
    putStrLn "   Usage: int2binIO 5  ->  Prints \"101\""
    putStrLn ""

    putStrLn "2. int2bit :: Int -> [Bit]"
    putStrLn "   Converts a decimal integer to a list of Bits."
    putStrLn "   Usage: int2bit 6  ->  [Zero, One, One]"
    putStrLn ""

    putStrLn "3. bin2int :: Int -> Int"
    putStrLn "   Takes an integer resembling binary (digits 1 and 0 only) and"
    putStrLn "   returns its true decimal value."
    putStrLn "   Usage: bin2int 101  ->  5"
    putStrLn ""

    putStrLn "4. binStr2bit :: String -> [Bit]"
    putStrLn "   Parses a string of '1's and '0's into a Bit list."
    putStrLn "   Usage: binStr2bit \"110\"  ->  [Zero, One, One]"
    putStrLn ""

    putStrLn "5. bin2bit :: Int -> [Bit]"
    putStrLn "   Takes an integer resembling binary, validates it, and converts"
    putStrLn "   it to a Bit list. Returns [Zero] if invalid."
    putStrLn "   Usage: bin2bit 110  ->  [Zero, One, One]"
    putStrLn ""

    putStrLn "6. binAdderIO :: Int -> Int -> IO ()"
    putStrLn "   Takes two integers resembling binary, adds them using the simulated"
    putStrLn "   ripple circuit, and prints the result as a binary string."
    putStrLn "   Usage: binAdderIO 10 11  ->  Prints \"101\""
    putStrLn ""

    putStrLn "7. binAdder :: Int -> Int -> [Bit]"
    putStrLn "   Takes two integers resembling binary and returns the sum as a"
    putStrLn "   Bit list using the simulated circuit."
    putStrLn "   Usage: binAdder 1 1  ->  [Zero, One]"
    putStrLn ""

    putStrLn "8. intAdder :: Int -> Int -> Int"
    putStrLn "   Takes two decimal integers, adds them via the simulated circuit,"
    putStrLn "   and returns the decimal sum."
    putStrLn "   Usage: intAdder 2 3  ->  5"
    putStrLn ""

    putStrLn "9. bit2int :: [Bit] -> Int"
    putStrLn "   Converts a list of Bits to a decimal integer."
    putStrLn "   Usage: bit2int [Zero, One]  ->  2"
    putStrLn ""

    putStrLn "10. bit2string :: [Bit] -> String"
    putStrLn "    Formats a Bit list into a raw string (LSB-Left)."
    putStrLn "    Usage: bit2string [Zero, One]  ->  \"01\""
    
    putStrLn "=================================="
    putStrLn "Press Enter to return to menu..."
    _ <- getLine
    return ()
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
    putStrLn "5. Exit"
    
    putStr "> "
    hFlush stdout -- Ensures the ">" appears before waiting for input
    
    choice <- getLine
    case choice of
        "1" -> runDecToBin >> menuLoop
        "2" -> runBinToDec >> menuLoop
        "3" -> runDecAdd   >> menuLoop
        "4" -> runBinAdd   >> menuLoop
        "5" -> putStrLn "Exiting simulation. Goodbye!"
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
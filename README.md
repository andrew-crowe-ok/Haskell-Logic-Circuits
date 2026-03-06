# Haskell-Logic-Circuits
A personal project in Haskell, written as an exercise/demonstration of concepts from CS-238L Computer Logic Design and CS-357 Declarative Programming.
Performs basic signed and unsigned arithmetic entirely through the use of types and functions that simulate bytes moving through circuits that are ultimately built from basic, 2-bit nand gates.

### Future updates to include:
    
    Reverse                        -> address the inconsistent usage of reverse at different points in the program, a source of bugs
    Overflow                       -> explicit overflow detection and handling
    Bitwise operations             -> more arithmetic operations and other concepts from CS-341L
    System clock & memory circuits -> latches, flip flops, registers
    Other                          -> improve the safeness and flexibility of existing systems

Upon loading the program, functions from every file are available to call directly. Or, type "main" to enter the main menu.

## Example: Gates.hs functions (must be outside of the main menu to use)

  ghci> nand One Zero
  One
  
  ghci> xor One One
  Zero

  ghci> andN [One, One, One]
  One

  ghci> orN [Zero, Zero, Zero, Zero, Zero, One, Zero, Zero]
  One


## Example: Utils.hs, Circuits.hs functions (must be outside of the main menu to use)

  ghci> stepZero = rippleAddN Zero (reverse [One, Zero, One, Zero]) (reverse [One, Zero, Zero])
  ghci> stepZero
  [Zero,One,One,One,Zero]
  
  ghci> stepOne = rippleAddN (head $ int2bit 0) (int2bit 10) (int2bit 4)
  ghci> stepOne
  [Zero,One,One,One,Zero]
  
  ghci> stepTwo = bit2intUnsigned stepOne
  ghci> stepTwo
  14
  
  ghci> stepThree = ((Zero :) . reverse . int2bit) stepTwo
  ghci> stepThree
  [Zero,One,One,One,Zero]
  
The second example is intended as a proof of the arithmetic and conversion logic. It also proves that I need to reevaluate my usage of reverse.
# Haskell-Logic-Circuits
Inspired by nand2tetris, this is a personal project written in Haskell that began as an exercise/demonstration of concepts from CS-238L Computer Logic Design and CS-357 Declarative Programming. It is a simulation of logic circuits built from primitive 2-bit nand gates. Currently, the ripple-carry adder is the most complex circuit available.  
***

<br>

Use ghci to run the program:
    
    > ghci main.hs
<br>

Upon loading main.hs, functions from every file are immediately available to call directly. See ***How it works*** below for instructions. Or, type ***main*** to enter the main menu for a simple walled garden experience.

    ghci> main
    
    ========================================
       HASKELL LOGIC CIRCUIT SIMULATOR
    ========================================
    
    SELECT AN OPERATION:
    1. Convert Decimal to Binary
    2. Convert Unsigned Binary to Decimal
    3. Unsigned Addition (0 to 255)
    4. Signed Addition (-128 to 127)
    5. View Utility Function Reference (Help)
    6. Exit
    >>
<br>
<br>

# How it works

Together with nand, these four primitive logic gate functions are the foundations on which the rest of the simulation is built.
(*not, and, or* from the Prelude are hidden so that I can use the names for my gates intead)

    import Prelude hiding (not, and, or)
    
    nand :: Bit -> Bit -> Bit
    nand One One = Zero
    nand _   _   = One
    
    not :: Bit -> Bit
    not x = nand x x
    
    and :: Bit -> Bit -> Bit
    and x y = not (nand x y)
    
    or :: Bit -> Bit -> Bit
    or x y = nand (not x) (not y)  
<br>

A simple, custom data type defines a bit.

    data Bit = Zero | One
        deriving (Show, Eq)  
<br>
<br>

Three examples follow. 
***Please note that the functions used here can only be accessed before entering or after exiting the main menu.***  

## Ex. \#1 Gates.hs

    ghci> nand One Zero
    One
  
    ghci> xor One One
    Zero

    ghci> andN [One, One, One]
    One

    ghci> orN [Zero, Zero, Zero, Zero, Zero, One, Zero, Zero]
    One
<br>
<br>

## Ex. #2 Circuits.hs, Utils.hs
    
    -- A ripple-carry adder constructed from primitive gates. Accepts a carry bit and two  
    -- lists of Bits that represent binary numbers. Input and output are little endian.
    rippleAddN :: Bit -> [Bit] -> [Bit] -> [Bit]
    rippleAddN c xs ys = ...
    
    ghci> rippleAddN Zero [Zero, One, Zero, One] [Zero, One, One]
    [Zero,Zero,Zero,Zero,One]
    
    
    
    -- Takes a non-negative decimal integer and produces a list of Bits.
    int2bit :: Int -> [Bit]
    int2bit n ...
    
    ghci> int2bit 25
    [One,Zero,Zero,One,One]
    
    
    
    -- Takes a binary literal and produces a safe list of Bits.
    bin2bit :: Int -> [Bit]
    bin2bit bin = ...
    
    ghci> bin2bit 11111000
    [Zero,Zero,Zero,One,One,One,One,One]
    
    
    
    -- Takes an unsigned list of Bits and produces a decimal integer. 
    bit2intUnsigned :: [Bit] -> Int
    bit2intUnsigned bits = ...
    
    ghci> bit2intUnsigned [Zero,Zero,Zero,One,One,One,One,One]
    248
<br>    
    
For convenience, the following two functions offer more straightforward access to the adder circuit by accepting binary literal and integer input and assuming 0 for the carry bit.

    binAdder :: Int -> Int -> [Bit]
    binAdder x y
        | x < 0 || y < 0 = error "Integers must be >= 0"
        | otherwise      = rippleAddN Zero (bin2bit x) (bin2bit y) 
    
    ghci> binAdder 1010 110
    [Zero,Zero,Zero,Zero,One]


    intAdder :: Int -> Int -> Int
    intAdder x y
        | x < 0 || y < 0 = error "Integers must be >= 0"
        | otherwise      = bit2intUnsigned $ rippleAddN Zero (int2bit x) (int2bit y)
        
    ghci> intAdder 10 6
    16
<br>
<br>

## Ex. #3 Proof
Intended as a sort of proof of correct arithmetic and type conversions.
    
### stepZero
A simple demonstration of the ripple-carry adder. To improve readability, my input lists are written in big endian, then reversed so that they are read correctly by the adder. In this example, it adds 1010 (10) and 110 (6) with a 0 carry bit. ***The output is in little endian.*** Therefore, it should be read as the binary literal 10000 (16).  

    
    ghci> stepZero = rippleAddN Zero (reverse [One, Zero, One, Zero]) (reverse [One, One, Zero])
    ghci> stepZero
    [Zero,Zero,Zero,Zero,One] 

### stepOne    
 Functionally identical to stepZero, but uses binAdder. This demonstrates a correct conversion from binary literal to bits.
 
    ghci> stepOne = binAdder 1010 110
    ghci> stepOne
    [Zero,Zero,Zero,Zero,One]

### stepTwo
Uses bit2intUnsigned to convert the output of stepOne to a decimal.

    ghci> stepTwo = bit2intUnsigned stepOne
    ghci> stepTwo
    16

### stepThree
Uses int2bit on the output of stepTwo, bringing us full circle back to stepZero. 

    ghci> stepThree = int2bit stepTwo
    ghci> stepThree
    [Zero,Zero,Zero,Zero,One]
<br>
<br>

# Future Improvements and Expansions

- **Additional Circuits**
    - multiplication
    - comparator
    - latch
    - flip flop
    - registers
    - system clock
- **CS-341L** 
    - bit shifting
- **AI**
    - review and rewrite AI-generated portions of the code 
    - this includes all of Byte.hs and Tests.hs, parts of main.hs
- **Other**
    - improve the safeness and flexibility of existing systems
<br>
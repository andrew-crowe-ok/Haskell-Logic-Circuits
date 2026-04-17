{-
Contains custom data types for simulation of binary data.
-}

module Logic.Types where
import Data.Sequence (Seq)

-- [Bit] are always ordered from LSB on the left to MSB on the right
data Bit = Zero | One 
  deriving (Show, Eq)

-- A distinct type for an 8-bit Word. 
-- It wraps the raw list so you can't accidentally use it as a variable-length list.
newtype Byte = Byte [Bit] 
  deriving (Show, Eq)

newtype SrLatch = SrLatch Bit
  deriving (Show, Eq)

newtype DFlipFlop = DFlipFlop Bit
  deriving (Show, Eq)

newtype Register8 = Register8 Byte
  deriving (Show, Eq)

type Memory = [(Int, Byte)]

data CpuState = CpuState {
  accumulator    :: Byte,   -- The main register for arithmetic and logic operations
  programCounter :: Int,    -- The address of the next instruction to be executed
  memory         :: Memory  -- The memory contents
} deriving (Eq, Show)
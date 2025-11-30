module Logic.Types where

-- [Bit] are always ordered from LSB on the left to MSB on the right
data Bit = Zero | One 
    deriving (Show, Eq)
    
-- TODO: data Byte
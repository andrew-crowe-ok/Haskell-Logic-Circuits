module Logic.Gates where

import Prelude hiding (not, and, or) 
import Logic.Types


-- 2-Input Gates

nand :: Bit -> Bit -> Bit
nand One One = Zero
nand _   _   = One


not :: Bit -> Bit
not x = nand x x


and :: Bit -> Bit -> Bit
and x y = not (nand x y)


or :: Bit -> Bit -> Bit
or x y = nand (not x) (not y)

xor :: Bit -> Bit -> Bit
xor x y = and (or x y) (nand x y)


-- N-Input Gates

andN :: [Bit] -> Bit
andN inputs = foldr and One inputs

orN :: [Bit] -> Bit
orN inputs = foldr or Zero inputs

nandN :: [Bit] -> Bit
nandN inputs = not (andN inputs)

xorN :: [Bit] -> Bit
xorN inputs = foldr xor Zero inputs

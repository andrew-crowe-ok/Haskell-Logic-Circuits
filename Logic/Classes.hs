module Logic.Classes where

import Logic.Types ( Bit(..) )

-- Define the typeclass interface
class Arithmetic a where
    add :: a -> a -> a
    sub :: a -> a -> a

class Sequential a where
  update :: Bit -> Bit -> a -> a
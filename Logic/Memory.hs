module Logic.Memory where

import Logic.Types
import Logic.Classes
import qualified Logic.Gates as G
import Data.Array.Byte (ByteArray)

instance Sequential SrLatch where
  update :: Bit -> Bit -> SrLatch -> SrLatch
  update s r (SrLatch q) =
      SrLatch (srLatch s r q)

instance Sequential DFlipFlop where
  update :: Bit -> Bit -> DFlipFlop -> DFlipFlop
  update d clock (DFlipFlop q) =
      DFlipFlop (dFlipFlop d clock q)

srLatch :: Bit -> Bit -> Bit -> Bit
srLatch s r qPrev = qNext
  where
    qNot  = G.not (G.or s qPrev)
    qNext = G.not (G.or r qNot)

dFlipFlop :: Bit -> Bit -> Bit -> Bit
dFlipFlop d clock = srLatch s r
  where
    s = G.and d clock
    r = G.and (G.not d) clock

register8 :: Byte -> Bit -> Byte -> Byte
register8 (Byte dBits) clock (Byte qBits) =
    Byte (zipWith (`dFlipFlop` clock) dBits qBits)
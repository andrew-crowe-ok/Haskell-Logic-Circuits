module Logic.CPU where

import Logic.Types (Memory, CpuState(..)) 
import Logic.Bit   ( Bit(..) )  
import Logic.Byte  ( Byte(..) )  

initCpu :: Memory -> CpuState
initCpu initialMemory = CpuState {
  accumulator    = Byte (replicate 8 Zero),
  programCounter = 0,
  memory         = initialMemory
}
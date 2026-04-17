module Logic.CPU where

import Control.Monad.State
import Logic.Types


initCpu :: Memory -> CpuState
initCpu initialMemory = CpuState {
  accumulator    = Byte (replicate 8 Zero),
  programCounter = 0,
  memory         = initialMemory
}
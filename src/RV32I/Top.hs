module RV32I.Top where

import RV32I.CPU
import RV32I.RAM
import RV32I.Register
import RV32I.Programs.Fib
import RV32I.Programs.Example (addImm)
import Clash.Signal (Signal, System, HiddenClockResetEnable, SystemClockResetEnable)
import Clash.Sized.Signed (Signed)
import Prelude (fmap)


top :: HiddenClockResetEnable dom => Signal dom Registers
-- top = cpu (zeroRegisterCPU (Ptr 112)) (programmedRAM (fibonacci 8))
top = cpu initCPUState (programmedRAM addImm)

hardwareTranslate :: Registers -> Signed 32
hardwareTranslate = a0

topEntity :: SystemClockResetEnable => Signal System (Signed 32)
topEntity = fmap hardwareTranslate top

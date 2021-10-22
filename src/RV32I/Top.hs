module RV32I.Top where

import RV32I.CPU
import RV32I.RAM
import RV32I.Register
import RV32I.Programs.Fib
import Clash.Signal (Signal, System, HiddenClockResetEnable)
import Clash.Sized.Signed (Signed)
import Prelude (fmap)


top :: HiddenClockResetEnable dom => Signal dom Registers
top = cpu (zeroRegisterCPU (Ptr 112)) (programmedRAM (fibonacci 8))

hardwareTranslate :: Registers -> Signed 32
hardwareTranslate regs = a0 regs

-- convert Register.A5 to Bit / BitVector / Signed 32 ?
-- topEntity accept each type?
-- and topEntity :: Signal System (BitVector x)

topEntity :: HiddenClockResetEnable dom => Signal dom (Signed 32)
topEntity = fmap hardwareTranslate top

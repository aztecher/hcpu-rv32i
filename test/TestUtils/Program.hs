module TestUtils.Program where

import RV32I.Instruction
import RV32I.Register
import RV32I.RAM
import RV32I.CPU
import Clash.Signal (Signal, System, sample)
import Clash.Sized.Vector(Vec)
import GHC.TypeLits

calculateDefaultSetup inst = calculateDefaultSetupWithN 100 inst

calculateDefaultSetupWithN n inst =
  let registers = Prelude.take n $ sample (cpu initCPUState (programmedRAM inst) :: Signal System Registers)
  in Prelude.last registers


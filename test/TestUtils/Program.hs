module TestUtils.Program where

import RV32I.Instruction
import RV32I.Register
import RV32I.RAM
import RV32I.CPU
import Clash.Signal (Signal, System, sample)
import Clash.Sized.Vector(Vec)
import GHC.TypeLits

calculateDefaultSetup ::
  (KnownNat n, KnownNat m, (n + m) ~ 1000)
    => Vec n Instruction
    -> Registers
calculateDefaultSetup inst = calculateDefaultSetupWithN 100 inst

calculateDefaultSetupWithN ::
  (KnownNat n, KnownNat m, (n + m) ~ 1000)
    => Int
    -> Vec n Instruction
    -> Registers
calculateDefaultSetupWithN n inst =
  let registers = Prelude.take n $ sample (cpu initCPUState (programmedRAM inst) :: Signal System Registers)
  in Prelude.last registers


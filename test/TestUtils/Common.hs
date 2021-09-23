module TestUtils.Common where

import RV32I.RV32I as RV32I
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, repeat)
import Prelude hiding (repeat)

obviously :: [Int] -> Bool
obviously _ = True


-- helper functions for unit test

propTag :: String
propTag = "[PROP] "

unitTag :: String
unitTag = "[Unit] "


-- helper functions for property-based-testing
zeroRAM :: RAM
zeroRAM = (RAM (repeat (Word32 0)))

zeroRegisters :: Registers
zeroRegisters = (Registers 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (Ptr 0))

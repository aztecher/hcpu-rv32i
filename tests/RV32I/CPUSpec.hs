module RV32I.CPUSpec where

import RV32I.Register
import RV32I.Word
import RV32I.CPU
import Clash.Sized.BitVector (Bit, BitVector)
import Clash.Sized.Signed (Signed)
import Clash.Class.BitPack (pack)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC
import TestUtils.Common
import Prelude (map, (<>), ($), (==), (.), Int, Bool(..))


-- test readRegister / writeRegister
test_read_write_Register :: Registers -> Register -> Signed 32 -> Signed 32
test_read_write_Register regs reg val = readRegister (writeRegister regs reg val) reg

-- test_replaceFromMsr
test_replaceFromMsr :: BitVector 32 -> Int -> Bit -> BitVector 32
test_replaceFromMsr = replaceFromMsr


spec :: Spec
spec = do
  -- UnitTest
  describe (unitTag <> "readRegister . writeRegister") $ do
    it "check writeRegister -> readRegister is match" $ do
      test_read_write_Register zeroRegisters Zero 1 `shouldBe` 1
  describe (unitTag <> "replaceFromMsr") $ do
    it "test replaceFromMsr" $ do
      test_replaceFromMsr (1 :: BitVector 32) 31 1 `shouldBe` (pack (-1 :: Signed 32))

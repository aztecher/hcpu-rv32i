module RV32I.Programs.ExampleSpec where

import RV32I.Register
import RV32I.Format
import RV32I.RAM
import RV32I.CPU
import Clash.Sized.BitVector (Bit, BitVector)
import Clash.Sized.Signed (Signed)
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, repeat)
import RV32I.Programs.Example as Example
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC

import TestUtils.Common as UtilCommon
import TestUtils.Program as UtilProgram

spec :: Spec
spec = do
  -- Property based test
  describe (propTag <> "test") $ do
    prop "property based testing" UtilCommon.obviously
  -- UnitTest
  describe (unitTag <> "addImm") $ do
    it "check addImm" $ do
      UtilProgram.calculateDefaultSetup addImm `shouldBe` zeroRegisters
        {
          s0fp = 3,
          s1 = 1,
          s2 = 2,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "addPlusMinus") $ do
    it "check addPlusMinus" $ do
      UtilProgram.calculateDefaultSetup addPlusMinus `shouldBe` zeroRegisters
        {
          s2 = 1,
          s3 = -1,
          s4 = 2,
          s5 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "subImm") $ do
    it "check subImm" $ do
      UtilProgram.calculateDefaultSetup subImm `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 2,
          s3 = -1,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "andImm") $ do
    it "check andImm" $ do
      UtilProgram.calculateDefaultSetup andImm `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "andDiff") $ do
    it "check andDiff" $ do
      UtilProgram.calculateDefaultSetup andDiff `shouldBe` zeroRegisters
        {
          s1 = 3,
          s2 = 5,
          s3 = 1,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "orImm") $ do
    it "check orImm" $ do
      UtilProgram.calculateDefaultSetup orImm `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "orDiff") $ do
    it "check orDiff" $ do
      UtilProgram.calculateDefaultSetup orDiff `shouldBe` zeroRegisters
        {
          s1 = 3,
          s2 = 5,
          s3 = 7,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "xorImm") $ do
    it "check xorImm" $ do
      UtilProgram.calculateDefaultSetup xorImm `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 0,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "xorDiff") $ do
    it "check xorDiff" $ do
      UtilProgram.calculateDefaultSetup xorDiff `shouldBe` zeroRegisters
        {
          s1 = 3,
          s2 = 5,
          s3 = 6,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "sll") $ do
    it "check sll" $ do
      UtilProgram.calculateDefaultSetup sll `shouldBe` zeroRegisters
        {
          s1 = 3,
          s2 = 4,
          s3 = 48,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "slliMsbZero") $ do
    it "check slliMsbZero" $ do
      UtilProgram.calculateDefaultSetup slliMsbZero `shouldBe` zeroRegisters
        {
          s1 = 3,
          s2 = 48,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "slliMsbOne") $ do
    it "check slliMsbOne" $ do
      UtilProgram.calculateDefaultSetup slliMsbOne `shouldBe` zeroRegisters
        {
          s1 = 3,
          s2 = 3,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "sraPlus") $ do
    it "check sraPlus" $ do
      UtilProgram.calculateDefaultSetup sraPlus `shouldBe` zeroRegisters
        {
          s1 = 1024,
          s2 = 5,
          s3 = 32,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "sraMinus") $ do
    it "check sraMinus" $ do
      UtilProgram.calculateDefaultSetup sraMinus `shouldBe` zeroRegisters
        {
          s1 = -1024,
          s2 = 5,
          s3 = -32,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "sraiPlus") $ do
    it "check sraiPlus" $ do
      UtilProgram.calculateDefaultSetup sraiPlus `shouldBe` zeroRegisters
        {
          s1 = 1024,
          s2 = 32,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "sraiMinus") $ do
    it "check sraiMinus" $ do
      UtilProgram.calculateDefaultSetup sraiMinusShift `shouldBe` zeroRegisters
        {
          s1 = 1024,
          s2 = 1024,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "srlPlus") $ do
    it "check srlPlus" $ do
      UtilProgram.calculateDefaultSetup srlPlus `shouldBe` zeroRegisters
        {
          s1 = 1024,
          s2 = 5,
          s3 = 32,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "srlMinus") $ do
    it "check srlMinus" $ do
      UtilProgram.calculateDefaultSetup srlMinus `shouldBe` zeroRegisters
        {
          s1 = -1024,
          s2 = 5,
          s3 = 134217696,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "srliPlus") $ do
    it "check srliPlus" $ do
      UtilProgram.calculateDefaultSetup srliPlus `shouldBe` zeroRegisters
        {
          s1 = 1024,
          s2 = 32,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "srliMinusShift") $ do
    it "check srliMinusShift" $ do
      UtilProgram.calculateDefaultSetup srliMinusShift `shouldBe` zeroRegisters
        {
          s1 = 1024,
          s2 = 1024,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "jalr") $ do
    it "check jalr" $ do
      UtilProgram.calculateDefaultSetup jalr `shouldBe` zeroRegisters
        {
          s1 = 32,
          s2 = 64,
          pc = Ptr . Offset $ 32
        }
  describe (unitTag <> "sltImmLt") $ do
    it "check sltImmLt" $ do
      UtilProgram.calculateDefaultSetup sltImmLt `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "sltImmGt") $ do
    it "check sltImmGt" $ do
      UtilProgram.calculateDefaultSetup sltImmGt `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 0,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "sltDiffLt") $ do
    it "check sltDiffLt" $ do
      UtilProgram.calculateDefaultSetup sltDiffLt `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 2,
          s3 = 1,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "sltDiffGt") $ do
    it "check sltDiffGt" $ do
      UtilProgram.calculateDefaultSetup sltDiffGt `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = -2,
          s3 = 0,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "sltiuLt") $ do
    it "check sltiuLt" $ do
      UtilProgram.calculateDefaultSetup sltiuLt `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "sltiuGeq") $ do
    it "check sltiuGeq" $ do
      UtilProgram.calculateDefaultSetup sltiuGeq `shouldBe` zeroRegisters
        {
          s1 = 8,
          s2 = 0,
          s3 = 96,
          pc = Ptr . Offset $ 64
        }
  describe (unitTag <> "sltuDiffLt") $ do
    it "check sltuDiffLt" $ do
      UtilProgram.calculateDefaultSetup sltuDiffLt `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 15,
          s3 = 1,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "sltuDiffGt") $ do
    it "check sltuDiffGt" $ do
      UtilProgram.calculateDefaultSetup sltuDiffGt `shouldBe` zeroRegisters
        {
          s1 = 12,
          s2 = 8,
          s3 = 0,
          s4 = 128,
          pc = Ptr . Offset $ 96
        }

  describe (unitTag <> "sblb") $ do
    it "check sblb" $ do
      UtilProgram.calculateDefaultSetup sblb `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "sblbOverByte") $ do
    it "check sblbOverByte" $ do
      UtilProgram.calculateDefaultSetup sblbOverByte `shouldBe` zeroRegisters
        {
          s1 = 256,
          s2 = 0,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "shlh") $ do
    it "check shlh" $ do
      UtilProgram.calculateDefaultSetup shlh `shouldBe` zeroRegisters
        {
          s1 = 256,
          s2 = 256,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "shlhOver2Byte") $ do
    it "check shlhOver2Byte" $ do
      UtilProgram.calculateDefaultSetup shlhOver2Byte `shouldBe` zeroRegisters
        {
          s1 = 0,
          s2 = 0,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "swlw") $ do
    it "check swlw" $ do
      UtilProgram.calculateDefaultSetup swlw `shouldBe` zeroRegisters
        {
          s1 = 256,
          s2 = 256,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "sblbu") $ do
    it "check sblbu" $ do
      UtilProgram.calculateDefaultSetup sblbu `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "sblbuMsbOne") $ do
      it "check sblbuMsbOne" $ do
        UtilProgram.calculateDefaultSetup sblbuMsbOne `shouldBe` zeroRegisters
          {
            s1 = 8,
            s2 = 8,
            s3 = 128,
            pc = Ptr . Offset $ 96
          }
  describe (unitTag <> "shlhu") $ do
    it "check shlhu" $ do
      UtilProgram.calculateDefaultSetup shlhu `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "shlhuMsbOne") $ do
    it "check shlhuMsbOne" $ do
      UtilProgram.calculateDefaultSetup shlhuMsbOne `shouldBe` zeroRegisters
        {
          s1 = 128,
          s2 = 128,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "beq") $ do
    it "check beq" $ do
      UtilProgram.calculateDefaultSetup beq `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "beqNg") $ do
    it "check beqNg" $ do
      UtilProgram.calculateDefaultSetup beqNg `shouldBe` zeroRegisters
        {
          s1 = 2,
          s2 = 1,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "bne") $ do
    it "check bne" $ do
      UtilProgram.calculateDefaultSetup bne `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s3 = 128,
          pc = Ptr . Offset $ 96
        }
  describe (unitTag <> "bneNg") $ do
    it "check bneNg" $ do
      UtilProgram.calculateDefaultSetup bneNg `shouldBe` zeroRegisters
        {
          s1 = 0,
          s2 = 1,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bltLtPlus") $ do
    it "check bltLtPlus" $ do
      UtilProgram.calculateDefaultSetup bltLtPlus `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 2,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bltLtMinus") $ do
    it "check bltLtMinus" $ do
      UtilProgram.calculateDefaultSetup bltLtMinus `shouldBe` zeroRegisters
        {
          s1 = -2,
          s2 = -1,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bltGeqPlus") $ do
    it "check bltGeqPlus" $ do
      UtilProgram.calculateDefaultSetup bltGeqPlus `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 2,
          s3 = 160,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bltGeqMinus") $ do
    it "check bltGeqMinus" $ do
      UtilProgram.calculateDefaultSetup bltGeqMinus `shouldBe` zeroRegisters
        {
          s1 = -3,
          s2 = -2,
          s3 = 160,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bgeGePlus") $ do
    it "check bgeGePlus" $ do
      UtilProgram.calculateDefaultSetup bgeGePlus `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 1,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bgeGeMinus") $ do
    it "check bgeGeMinus" $ do
      UtilProgram.calculateDefaultSetup bgeGeMinus `shouldBe` zeroRegisters
        {
          s1 = -1,
          s2 = -2,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bgeLtPlus") $ do
    it "check bgeLtPlus" $ do
      UtilProgram.calculateDefaultSetup bgeLtPlus `shouldBe` zeroRegisters
        {
          s1 = 2,
          s2 = 2,
          s3 = 160,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bgeLtMinus") $ do
    it "check bgeLtMinus" $ do
      UtilProgram.calculateDefaultSetup bgeLtMinus `shouldBe` zeroRegisters
        {
          s1 = -1,
          s2 = -1,
          s3 = 160,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bltult") $ do
    it "check bltult" $ do
      UtilProgram.calculateDefaultSetup bltult `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 2,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bltuge") $ do
    it "check bltuge" $ do
      UtilProgram.calculateDefaultSetup bltuge `shouldBe` zeroRegisters
        {
          s1 = 1,
          s2 = 2,
          s3 = 160,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bgeuge") $ do
    it "check bgeuge" $ do
      UtilProgram.calculateDefaultSetup bgeuge `shouldBe` zeroRegisters
        {
          s1 = 2,
          s2 = 2,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "bgeult") $ do
    it "check bgeult" $ do
      UtilProgram.calculateDefaultSetup bgeult `shouldBe` zeroRegisters
        {
          s1 = 2,
          s2 = 2,
          s3 = 160,
          s4 = 192,
          pc = Ptr . Offset $ 160
        }
  describe (unitTag <> "luiPlus") $ do
    it "check luiPlus" $ do
      UtilProgram.calculateDefaultSetup luiPlus `shouldBe` zeroRegisters
        {
          s1 = 20480,
          s2 = 64,
          pc = Ptr . Offset $ 32
        }
  describe (unitTag <> "luiMinus") $ do
    it "check luiMinus" $ do
      UtilProgram.calculateDefaultSetup luiMinus `shouldBe` zeroRegisters
        {
          s1 = -20480,
          s2 = 64,
          pc = Ptr . Offset $ 32
        }
  describe (unitTag <> "auipc") $ do
    it "check auipc" $ do
      UtilProgram.calculateDefaultSetup auipc `shouldBe` zeroRegisters
        {
          s1 = 20480,
          s2 = 64,
          pc = Ptr . Offset $ 32
        }
  describe (unitTag <> "jal") $ do
    it "check jal" $ do
      UtilProgram.calculateDefaultSetup jal `shouldBe` zeroRegisters
        {
          s1 = 32,
          pc = Ptr . Offset $ 0
        }

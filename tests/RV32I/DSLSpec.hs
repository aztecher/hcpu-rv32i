module RV32I.DSLSpec where

import RV32I.DSL as DSL
import RV32I.Register
import RV32I.Format
import RV32I.Word
import RV32I.Instruction
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC
import TestUtils.Common as UtilCommon
import Prelude (map, (<>), ($), (==), (.), Bool(..))

spec :: Spec
spec = do
  -- UnitTest
  -- R-Type Format
  describe (unitTag <> "add") $ do
    it "check DSL 'add'" $ do
      DSL.add S2 S1 S0 `shouldBe` R (RArith ADD S0 S1 S2)
  describe (unitTag <> "sub") $ do
    it "check DSL 'sub'" $ do
      DSL.sub S2 S1 S0 `shouldBe` R (RArith SUB S0 S1 S2)
  describe (unitTag <> "and") $ do
    it "check DSL 'and'" $ do
      DSL.and S2 S1 S0 `shouldBe` R (RArith AND S0 S1 S2)
  describe (unitTag <> "or") $ do
    it "check DSL 'or'" $ do
      DSL.or S2 S1 S0 `shouldBe` R (RArith OR S0 S1 S2)
  describe (unitTag <> "xor") $ do
    it "check DSL 'xor'" $ do
      DSL.xor S2 S1 S0 `shouldBe` R (RArith XOR S0 S1 S2)
  describe (unitTag <> "sll") $ do
    it "check DSL 'sll'" $ do
      DSL.sll S2 S1 S0 `shouldBe` R (RArith SLL S0 S1 S2)
  describe (unitTag <> "slt") $ do
    it "check DSL 'slt'" $ do
      DSL.slt S2 S1 S0 `shouldBe` R (RArith SLT S0 S1 S2)
  describe (unitTag <> "sltu") $ do
    it "check DSL 'sltu'" $ do
      DSL.sltu S2 S1 S0 `shouldBe` R (RArith SLTU S0 S1 S2)
  describe (unitTag <> "sra") $ do
    it "check DSL 'sra'" $ do
      DSL.sra S2 S1 S0 `shouldBe` R (RArith SRA S0 S1 S2)
  describe (unitTag <> "srl") $ do
    it "check DSL 'srl'" $ do
      DSL.srl S2 S1 S0 `shouldBe` R (RArith SRL S0 S1 S2)
  -- I-Type Format
  describe (unitTag <> "addi") $ do
    it "check DSL 'addi'" $ do
      DSL.addi S2 Zero 2 `shouldBe` I (IArith ADDI (Word12 2) Zero S2)
  describe (unitTag <> "slti") $ do
    it "check DSL 'slti'" $ do
      DSL.slti S2 S1 2 `shouldBe` I (IArith SLTI (Word12 2) S1 S2)
  describe (unitTag <> "sltiu") $ do
    it "check DSL 'sltiu'" $ do
      DSL.sltiu S2 S1 2 `shouldBe` I (IArith SLTIU (Word12 2) S1 S2)
  describe (unitTag <> "xori") $ do
    it "check DSL 'xori'" $ do
      DSL.xori S2 S1 2 `shouldBe` I (IArith XORI (Word12 2) S1 S2)
  describe (unitTag <> "ori") $ do
    it "check DSL 'ori'" $ do
      DSL.ori S2 S1 2 `shouldBe` I (IArith ORI (Word12 2) S1 S2)
  describe (unitTag <> "andi") $ do
    it "check DSL 'andi'" $ do
      DSL.andi S2 Zero 2 `shouldBe` I (IArith ANDI (Word12 2) Zero S2)
  describe (unitTag <> "slli") $ do
    it "check DSL 'slli'" $ do
      DSL.slli S2 S1 2 `shouldBe` I (IShift SLLI (Word5 2) S1 S2)
  describe (unitTag <> "srai") $ do
    it "check DSL 'srai'" $ do
      DSL.srai S2 S1 2 `shouldBe` I (IShift SRAI (Word5 2) S1 S2)
  describe (unitTag <> "srli") $ do
    it "check DSL 'srli'" $ do
      DSL.srli S2 S1 2 `shouldBe` I (IShift SRLI (Word5 2) S1 S2)
  describe (unitTag <> "jalr") $ do
    it "check DSL 'jalr'" $ do
      DSL.jalr S2 2 S1 `shouldBe` I (IJalr JALR (Word12 2) S1 S2)
  describe (unitTag <> "lb") $ do
    it "check DSL 'lb'" $ do
      DSL.lb S2 2 S1 `shouldBe` I (ILoad LB (Word12 2) S1 S2)
  describe (unitTag <> "lh") $ do
    it "check DSL 'lh'" $ do
      DSL.lh S2 2 S1 `shouldBe` I (ILoad LH (Word12 2) S1 S2)
  describe (unitTag <> "lw") $ do
    it "check DSL 'lw'" $ do
      DSL.lw S2 2 S1 `shouldBe` I (ILoad LW (Word12 2) S1 S2)
  describe (unitTag <> "lbu") $ do
    it "check DSL 'lbu'" $ do
      DSL.lbu S2 2 S1 `shouldBe` I (ILoad LBU (Word12 2) S1 S2)
  describe (unitTag <> "lhu") $ do
    it "check DSL 'lhu'" $ do
      DSL.lhu S2 2 S1 `shouldBe` I (ILoad LHU (Word12 2) S1 S2)
  -- S-Type Format
  describe (unitTag <> "sb") $ do
    it "check DSL 'sb'" $ do
      DSL.sb S2 900 S1 `shouldBe` S (SStore SB (Word7 28) S2 S1 (Word5 4))
  describe (unitTag <> "sh") $ do
    it "check DSL 'sh'" $ do
      DSL.sh S2 900 S1 `shouldBe` S (SStore SH (Word7 28) S2 S1 (Word5 4))
  describe (unitTag <> "sw") $ do
    it "check DSL 'sw'" $ do
      DSL.sw S2 900 S1 `shouldBe` S (SStore SW (Word7 28) S2 S1 (Word5 4))
  -- B-Type Format
  describe (unitTag <> "beq") $ do
    it "check DSL 'beq'" $ do
      DSL.beq S2 S1 2 `shouldBe` B (BBranch BEQ (Word7 0) S1 S2 (Word5 2))
  describe (unitTag <> "bge") $ do
    it "check DSL 'bge'" $ do
      DSL.bge S2 S1 2 `shouldBe` B (BBranch BGE (Word7 0) S1 S2 (Word5 2))
  describe (unitTag <> "bgeu") $ do
    it "check DSL 'bgeu'" $ do
      DSL.bgeu S2 S1 2 `shouldBe` B (BBranch BGEU (Word7 0) S1 S2 (Word5 2))
  describe (unitTag <> "blt") $ do
    it "check DSL 'blt'" $ do
      DSL.blt S2 S1 2 `shouldBe` B (BBranch BLT (Word7 0) S1 S2 (Word5 2))
  describe (unitTag <> "bltu") $ do
    it "check DSL 'bltu'" $ do
      DSL.bltu S2 S1 2 `shouldBe` B (BBranch BLTU (Word7 0) S1 S2 (Word5 2))
  describe (unitTag <> "bne") $ do
    it "check DSL 'bne'" $ do
      DSL.bne S2 S1 2 `shouldBe` B (BBranch BNE (Word7 0) S1 S2 (Word5 2))
  -- U-Type Format
  describe (unitTag <> "lui") $ do
    it "check DSL 'lui'" $ do
      DSL.lui S1 5 `shouldBe` U (UArith LUI (Word20 5) S1)

  describe (unitTag <> "auipc") $ do
    it "check DSL 'auipc'" $ do
      DSL.auipc S1 5 `shouldBe` U (UArith AUIPC (Word20 5) S1)
  -- J-Type Format
  describe (unitTag <> "jal") $ do
    it "check DSL 'jal'" $ do
      DSL.jal S4 3 `shouldBe` J (JJal JAL (Word20 3) S4)

module RV32I.RV32ISpec where

import Clash.Sized.BitVector (Bit, BitVector)
import Clash.Sized.Signed (Signed)
import RV32I.RV32I as RV32I
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC

import TestUtils.Common

instance Arbitrary Register where
  arbitrary = QC.chooseEnum (Zero, T6)

instance Arbitrary Word32 where
  arbitrary = do
    gen <- QC.choose (-2147483648 :: Integer, 2147483647)
    return $ Word32 (fromIntegral gen :: Signed 32)

instance Arbitrary Word20 where
  arbitrary = do
    gen <- QC.choose (-524288 :: Integer, 524287)
    return $ Word20 (fromIntegral gen :: Signed 20)

instance Arbitrary Word16 where
  arbitrary = do
    gen <- QC.choose (-32768 :: Integer, 32767)
    return $ Word16 (fromIntegral gen :: Signed 16)

instance Arbitrary Word12 where
  arbitrary = do
    gen <- QC.choose (-2048 :: Integer, 2047)
    return $ Word12 (fromIntegral gen :: Signed 12)

instance Arbitrary Word8 where
  arbitrary = do
    gen <- QC.choose (-128 :: Integer, 127)
    return $ Word8 (fromIntegral gen :: Signed 8)

instance Arbitrary Word7 where
  arbitrary = do
    gen <- QC.choose (-64 :: Integer, 63)
    return $ Word7 (fromIntegral gen :: Signed 7)

instance Arbitrary Word5 where
  arbitrary = do
    gen <- QC.choose (-16 :: Integer, 15)
    return $ Word5 (fromIntegral gen :: Signed 5)

instance Arbitrary Word4 where
  arbitrary = do
    gen <- QC.choose (-8 :: Integer, 7)
    return $ Word4 (fromIntegral gen :: Signed 4)

instance Arbitrary RArith where
  arbitrary = QC.chooseEnum (ADD, SLTU)

instance Arbitrary IArith where
  arbitrary = QC.chooseEnum (ADDI, ANDI)

instance Arbitrary IShift where
  arbitrary = QC.chooseEnum (SLLI, SRLI)

instance Arbitrary IJalr where
  arbitrary = QC.chooseEnum (JALR, JALR)

instance Arbitrary ILoad where
  arbitrary = QC.chooseEnum (LB, LHU)

instance Arbitrary IFence where
  arbitrary = QC.chooseEnum (FENCE, FENCE)

instance Arbitrary IFencei where
  arbitrary = QC.chooseEnum (FENCEI, FENCEI)

instance Arbitrary IEnv where
  arbitrary = QC.chooseEnum (EBREAK, ECALL)

instance Arbitrary ICsr where
  arbitrary = QC.chooseEnum (CSRRC, CSRRW)

instance Arbitrary ICsri where
  arbitrary = QC.chooseEnum (CSRRCI, CSRRWI)

instance Arbitrary SStore where
  arbitrary = QC.chooseEnum (SB, SW)

instance Arbitrary BBranch where
  arbitrary = QC.chooseEnum (BEQ, BGEU)

instance Arbitrary UArith where
  arbitrary = QC.chooseEnum (LUI, AUIPC)

instance Arbitrary JJal where
  arbitrary = QC.chooseEnum (JAL, JAL)

instance Arbitrary RFormat where
  arbitrary = do
    arith <- arbitrary :: Gen RArith
    reg1 <- arbitrary :: Gen Register
    reg2 <- arbitrary :: Gen Register
    reg3 <- arbitrary :: Gen Register
    return $ RArith arith reg1 reg2 reg3

instance Arbitrary IFormat where
  arbitrary = do
    iarith <- arbitrary :: Gen IArith
    ishift <- arbitrary :: Gen IShift
    ijalr <- arbitrary :: Gen IJalr
    iload <- arbitrary :: Gen ILoad
    ifence <- arbitrary :: Gen IFence
    ifencei <- arbitrary :: Gen IFencei
    ienv <- arbitrary :: Gen IEnv
    icsr <- arbitrary :: Gen ICsr
    icsri <- arbitrary :: Gen ICsri
    word12 <- arbitrary :: Gen Word12
    word5 <- arbitrary :: Gen Word5
    word4 <- arbitrary :: Gen Word4
    reg <- arbitrary :: Gen Register
    elements
      [ (IArith iarith word12 reg reg),
        (IShift ishift word5 reg reg),
        (IJalr ijalr word12 reg reg),
        (ILoad iload word12 reg reg),
        (IFence ifence word4 word4),
        (IFencei ifencei),
        (IEnv ienv),
        (ICsr icsr word12 reg reg),
        (ICsri icsri word12 word5 reg)
      ]

instance Arbitrary SFormat where
  arbitrary = do
    sstore <- arbitrary :: Gen SStore
    word7 <- arbitrary :: Gen Word7
    reg1 <- arbitrary :: Gen Register
    reg2 <- arbitrary :: Gen Register
    word5 <- arbitrary :: Gen Word5
    return $ SStore sstore word7 reg1 reg2 word5

instance Arbitrary BFormat where
  arbitrary = do
    bbranch <- arbitrary :: Gen BBranch
    word7 <- arbitrary :: Gen Word7
    reg1 <- arbitrary :: Gen Register
    reg2 <- arbitrary :: Gen Register
    word5 <- arbitrary :: Gen Word5
    return $ BBranch bbranch word7 reg1 reg2 word5

instance Arbitrary UFormat where
  arbitrary = do
    uarith <- arbitrary :: Gen UArith
    word20 <- arbitrary :: Gen Word20
    reg <- arbitrary :: Gen Register
    return $ UArith uarith word20 reg

instance Arbitrary JFormat where
  arbitrary = do
    jjal <- arbitrary :: Gen JJal
    word20 <- arbitrary :: Gen Word20
    reg <- arbitrary :: Gen Register
    return $ JJal jjal word20 reg

instance Arbitrary Instruction where
  arbitrary = do
    rformat <- arbitrary :: Gen RFormat
    iformat <- arbitrary :: Gen IFormat
    sformat <- arbitrary :: Gen SFormat
    bformat <- arbitrary :: Gen BFormat
    uformat <- arbitrary :: Gen UFormat
    jformat <- arbitrary :: Gen JFormat
    elements
      [ (R rformat),
        (I iformat),
        (S sformat),
        (B bformat),
        (U uformat),
        (J jformat)
      ]


-- encode/decode from/to Register/BitVector5 will be satisfied identification.
decode_encode_register :: [Register] -> Bool
decode_encode_register xs = map (decodeRegister . encodeRegister) xs == xs

encode_decode_bitvector :: [BitVector 5] -> Bool
encode_decode_bitvector xs = map (encodeRegister . decodeRegister) xs == xs

-- encode/decode from/to Instruction/Word32 will be satisfied identification
decode_encode_instruction :: [Instruction] -> Bool
decode_encode_instruction xs = map (decodeInstruction . encodeInstruction) xs == xs

-- test other helper function of RV32I.RV32I

-- test readRAM / writeRAM
test_read_writeRAM :: RAM -> Ptr -> Word32 -> Word32
test_read_writeRAM ram ptr val = readRAM (writeRAM ram ptr val) ptr

-- test readRAM4Byte / writeRAM4Byte
test_read_writeRAM4Byte :: RAM -> Ptr -> Word32 -> Word32
test_read_writeRAM4Byte ram ptr val = readRAM4Byte (writeRAM4Byte ram ptr val) ptr

-- test readRAM2Byte / writeRAM2Byte
test_read_writeRAM2Byte :: RAM -> Ptr -> Word16 -> Word16
test_read_writeRAM2Byte ram ptr val = readRAM2Byte (writeRAM2Byte ram ptr val) ptr

-- test readRAM1Byte / writeRAM1Byte
test_read_writeRAM1Byte :: RAM -> Ptr -> Word8 -> Word8
test_read_writeRAM1Byte ram ptr val = readRAM1Byte (writeRAM1Byte ram ptr val) ptr

-- test_increment (check by pointer's address)
-- -- Ptr type Equality is ambiguous for GHC.Ptr
-- -- so, avoid this, check address
test_increment_ptr_address :: Ptr -> Signed 32
test_increment_ptr_address ptr = val
  where
    (Ptr val) = increment ptr

-- test_addptr
test_addptr_ptr_address :: Ptr -> Signed 32 -> Signed 32
test_addptr_ptr_address ptr offset = val
  where
    (Ptr val) = addptr ptr offset

-- test readRegister / writeRegister
test_read_write_Register :: Registers -> Register -> Signed 32 -> Signed 32
test_read_write_Register regs reg val = readRegister (writeRegister regs reg val) reg

-- test_replaceFromMsr
test_replaceFromMsr :: BitVector 32 -> Int -> Bit -> BitVector 32
test_replaceFromMsr = replaceFromMsr

spec :: Spec
spec = do
  -- Property based test
  describe (propTag <> "test") $ do
    prop "property based testing" obviously
  describe (propTag <> "decodeRegister . encodeRegister") $ do
    prop "check decode / encode Register identity property" $ decode_encode_register
  describe (propTag <> "encodeRegister . decodeRegister") $ do
    prop "check encode / decode BitVector5 identity property" $ encode_decode_bitvector
  describe (propTag <> "decodeInstruction . encodeInstruction") $ do
    prop "check decode / encode Instruction identity property" $ decode_encode_instruction
  -- UnitTest
  describe (unitTag <> "test") $ do
    it "unit testing" $ do
      1 `shouldBe` 1
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM -> readRAM is match" $ do
      test_read_writeRAM zeroRAM (Ptr 0) (Word32 (1 :: Signed 32)) `shouldBe` (Word32 (1 :: Signed 32))
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM4Byte -> readRAM4Byte is match" $ do
      test_read_writeRAM4Byte zeroRAM (Ptr 0) (Word32 (1 :: Signed 32)) `shouldBe` (Word32 (1 :: Signed 32))
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM2Byte -> readRAM2Byte is match" $ do
      test_read_writeRAM2Byte zeroRAM (Ptr 0) (Word16 (1 :: Signed 16)) `shouldBe` (Word16 (1 :: Signed 16))
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM1Byte -> readRAM1Byte is match" $ do
      test_read_writeRAM1Byte zeroRAM (Ptr 0) (Word8 (1 :: Signed 8)) `shouldBe` (Word8 (1 :: Signed 8))
  describe (unitTag <> "incement") $ do
    it "test increment function by check it's address" $ do
      test_increment_ptr_address (Ptr 0) `shouldBe` (1 :: Signed 32)
  describe (unitTag <> "addptr") $ do
    it "test addptr function by check it's address" $ do
      test_addptr_ptr_address (Ptr 0) (1 :: Signed 32) `shouldBe` (1 :: Signed 32)
  describe (unitTag <> "readRegister . writeRegister") $ do
    it "check writeRegister -> readRegister is match" $ do
      test_read_write_Register zeroRegisters Zero (1 :: Signed 32) `shouldBe` (1 :: Signed 32)

-- test_replaceFromMsr
-- describe (unitTag <> "replaceFromMsr") $ do
--   it "test replaceFromMsr" $ do
--     test_read_write_Register zeroRegisters Zero (1 :: Signed 32) `shouldBe` (1 :: Signed 32)

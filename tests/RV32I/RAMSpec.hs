module RV32I.RAMSpec where

import RV32I.Word
import RV32I.Format
import RV32I.Register
import RV32I.Instruction
import RV32I.RAM
import Data.Maybe (fromMaybe)
import Clash.Sized.BitVector (Bit, BitVector)
import Clash.Sized.Vector (repeat, Vec((:>), Nil), (++), concatBitVector#)
import Clash.Sized.Signed (Signed)
import Clash.Class.BitPack (unpack)
import Prelude hiding (repeat, (++))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC

import TestUtils.Common

-- Defualt variables
firstInst  = I (IArith ADDI (Word12 (-1 :: Signed 12)) S2 S3) :> Nil
secondInst = I (IArith ADDI (Word12 (2 :: Signed 12)) S2 S3) :> Nil
thirdInst  = R (RArith ADD S3 S4 S2) :> Nil
fourthInst = J (JJal JAL (Word20 128) S5) :> Nil
testProgram = firstInst ++ secondInst ++ thirdInst ++ fourthInst
_oneInstTo32Bit n = concatBitVector# $ fmap (word32ToBitVector . encodeInstruction) n

testRAM :: RAM
testRAM = programmedRAM testProgram

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

-- test increment
test_increment_ptr_address :: Ptr -> Signed 32
test_increment_ptr_address ptr = val
  where
    (Ptr val) = increment ptr

-- test addptr
test_addptr_ptr_address :: Ptr -> Signed 32 -> Signed 32
test_addptr_ptr_address ptr offset = val
  where
    (Ptr val) = addptr ptr offset

-- test Word32 <-> BitVector 32
convert_word_bitvector32 :: [BitVector 32] -> Bool
convert_word_bitvector32 xs = map (word32ToBitVector . bitVectorToWord32) xs == xs

convert_bitvector_word32 :: [Word32] -> Bool
convert_bitvector_word32 xs = map (bitVectorToWord32 . word32ToBitVector) xs == xs

-- test Word8 <-> BitVector 8
convert_word_bitvector8 :: [BitVector 8] -> Bool
convert_word_bitvector8 xs = map (word8ToBitVector . bitVectorToWord8) xs == xs

convert_bitvector_word8 :: [Word8] -> Bool
convert_bitvector_word8 xs = map (bitVectorToWord8 . word8ToBitVector) xs == xs


spec :: Spec
spec = do
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM -> readRAM is match" $ do
      test_read_writeRAM zeroRAM (Ptr 1) (Word32 (1 :: Signed 32)) `shouldBe` (Word32 (1 :: Signed 32))
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM4Byte -> readRAM4Byte is match" $ do
      test_read_writeRAM4Byte zeroRAM (Ptr 1) (Word32 (1 :: Signed 32)) `shouldBe` (Word32 (1 :: Signed 32))
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM2Byte -> readRAM2Byte is match" $ do
      test_read_writeRAM2Byte zeroRAM (Ptr 1) (Word16 (1 :: Signed 16)) `shouldBe` (Word16 (1 :: Signed 16))
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM1Byte -> readRAM1Byte is match" $ do
      test_read_writeRAM1Byte zeroRAM (Ptr 1) (Word8 (1 :: Signed 8)) `shouldBe` (Word8 (1 :: Signed 8))
  describe (unitTag <> "incement") $ do
    it "test increment function by check it's address" $ do
      test_increment_ptr_address (Ptr 0) `shouldBe` (4 :: Signed 32)
  describe (unitTag <> "addptr") $ do
    it "test addptr function by check it's address" $ do
      test_addptr_ptr_address (Ptr 1) 1 `shouldBe` (2 :: Signed 32)
  describe (propTag <> "word32ToBitVector . bitVectorToWord32") $ do
    prop "check word32ToBitVector / bitVectorToWord32" $ convert_word_bitvector32
  describe (propTag <> "bitVectorToWord32 . word32ToBitVector") $ do
    prop "check bitVectorToWord32 / word32ToBitVector" $ convert_bitvector_word32
  describe (propTag <> "word8ToBitVector . bitVectorToWord8") $ do
    prop "check word8ToBitVector / bitVectorToWord8" $ convert_word_bitvector32
  describe (propTag <> "bitVectorToWord8 . word8ToBitVector") $ do
    prop "check bitVectorToWord8 / word8ToBitVector" $ convert_bitvector_word8
  describe (unitTag <> "readRAM4Byte") $ do
    it "test readRAM4Byte (0~32)" $ do
      readRAM4Byte testRAM (Ptr 0)
        `shouldBe` Word32 (unpack (_oneInstTo32Bit firstInst))
    it "test readRAM4Byte (32~63)" $ do
      readRAM4Byte testRAM (Ptr 4)
        `shouldBe` Word32 (unpack (_oneInstTo32Bit secondInst))
    it "test readRAM4Byte (64~95)" $ do
      readRAM4Byte testRAM (Ptr 8)
        `shouldBe` Word32 (unpack (_oneInstTo32Bit thirdInst))
    it "test readRAM4Byte (96~127)" $ do
      readRAM4Byte testRAM (Ptr 12)
        `shouldBe` Word32 (unpack (_oneInstTo32Bit fourthInst))

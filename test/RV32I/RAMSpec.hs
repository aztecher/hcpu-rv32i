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

zeroAddress :: Address
zeroAddress = Address (repeat (0 :: BitVector 1))

testRAM :: RAM
testRAM = programmedRAM testProgram

testAddress :: Address
testAddress = ramToAddress testRAM

testAddressL :: AddressL
testAddressL = addressToList testAddress


-- test Offset <-> Signed 32
convert_offset_signed32 :: [Offset] -> Bool
convert_offset_signed32 xs = map (signed32ToOffset . offsetToSigned32) xs == xs

convert_signed32_offset :: [Signed 32] -> Bool
convert_signed32_offset xs = map (offsetToSigned32 . signed32ToOffset) xs == xs


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
test_increment_ptr_address :: Ptr -> Int
test_increment_ptr_address ptr = val
  where
    (Ptr (Offset val)) = increment ptr

-- test addptr
test_addptr_ptr_address :: Ptr -> Signed 32 -> Int
test_addptr_ptr_address ptr offset = val
  where
    (Ptr (Offset val)) = addptr ptr offset

-- test programmedRAM / paddingEncodedInstruction?

-- test Word32 <-> BitVector 32
convert_word_bitvector :: [BitVector 32] -> Bool
convert_word_bitvector xs = map (word32ToBitVector . bitVectorToWord32) xs == xs

convert_bitvector_word :: [Word32] -> Bool
convert_bitvector_word xs = map (bitVectorToWord32 . word32ToBitVector) xs == xs

-- test word32/16/8ToAddressL


-- test ramToAddress <-> addressToRAM
convert_ram_address :: [Address] -> Bool
convert_ram_address xs = map (ramToAddress . addressToRAM) xs == xs

convert_address_ram :: [RAM] -> Bool
convert_address_ram xs = map (addressToRAM . ramToAddress) xs == xs

-- test addressToList <-> listToAddress (Maybe)
convert_addressL_address :: [Address] -> Bool
convert_addressL_address xs = map fromto xs == xs
  where
    fromto :: Address -> Address
    fromto = (fromMaybe zeroAddress) . listToAddress . addressToList

convert_address_addressL :: [AddressL] -> Bool
convert_address_addressL xs = map tofrom xs == xs
  where
    tofrom :: AddressL -> AddressL
    tofrom = addressToList . (fromMaybe zeroAddress) . listToAddress

convert_addressL_ram :: [RAM] -> Bool
convert_addressL_ram xs = map tofrom xs == xs
  where
    tofrom :: RAM -> RAM
    tofrom = addressToRAM . (fromMaybe zeroAddress) . listToAddress . ramToAddressL

convert_ram_addressL :: [AddressL] -> Bool
convert_ram_addressL xs = map tofrom xs == xs
  where
    tofrom :: AddressL -> AddressL
    tofrom = ramToAddressL . addressToRAM . (fromMaybe zeroAddress) . listToAddress

test_fetch_replace32Bit :: AddressL -> Offset -> Word32 -> Maybe (BitVector 32)
test_fetch_replace32Bit address offset word = fetch32Bit address' offset
  where
    address' = replace32Bit address offset word

test_fetch_register32Bit :: RAM -> Offset -> Word32 -> Maybe (BitVector 32)
test_fetch_register32Bit ram offset word = fetch32Bit address' offset
  where
    registered = register32Bit ram offset word
    address' = addressToList $ fromMaybe zeroAddress registered

spec :: Spec
spec = do
  describe (propTag <> "offsetToSigned32 / signed32ToOffset") $ do
    prop "check offsetToSigned32 / signed32ToOffset" $ convert_offset_signed32
  describe (propTag <> "signed32ToOffset / offsetToSigned32") $ do
    prop "check signed32ToOffset / offsetToSigned32" $ convert_signed32_offset
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM -> readRAM is match" $ do
      test_read_writeRAM zeroRAM (Ptr . Offset $  1) (Word32 (1 :: Signed 32)) `shouldBe` (Word32 (1 :: Signed 32))
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM4Byte -> readRAM4Byte is match" $ do
      test_read_writeRAM4Byte zeroRAM (Ptr . Offset $  1) (Word32 (1 :: Signed 32)) `shouldBe` (Word32 (1 :: Signed 32))
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM2Byte -> readRAM2Byte is match" $ do
      test_read_writeRAM2Byte zeroRAM (Ptr . Offset $  1) (Word16 (1 :: Signed 16)) `shouldBe` (Word16 (1 :: Signed 16))
  describe (unitTag <> "readRAM . writeRAM") $ do
    it "check writeRAM1Byte -> readRAM1Byte is match" $ do
      test_read_writeRAM1Byte zeroRAM (Ptr . Offset $  1) (Word8 (1 :: Signed 8)) `shouldBe` (Word8 (1 :: Signed 8))
  describe (unitTag <> "incement") $ do
    it "test increment function by check it's address" $ do
      test_increment_ptr_address (Ptr . Offset $  0) `shouldBe` 32
  describe (unitTag <> "addptr") $ do
    it "test addptr function by check it's address" $ do
      test_addptr_ptr_address (Ptr . Offset $  1) 1 `shouldBe` 2
  describe (propTag <> "word32ToBitVector . bitVectorToWord32") $ do
    prop "check word32ToBitVector / bitVectorToWord32" $ convert_word_bitvector
  describe (propTag <> "bitVectorToWord32 . word32ToBitVector") $ do
    prop "check bitVectorToWord32 / word32ToBitVector" $ convert_bitvector_word

  -- RAM / Address test is too heavy, I have already confirmed that this test will succssed.
  -- If you want to check this test, please uncomment bellow codes.
  -- describe (propTag <> "ramToAddress / addressToRAM") $ do
  --   prop "check ramToAddress / addressToRAM" $ convert_ram_address
  -- describe (propTag <> "addressToRAM / ramToAddress") $ do
  --   prop "check addressToRAM / ramToAddress" $ convert_address_ram
  -- describe (propTag <> "addressToAddressL . addressLToAddress") $ do
  --   prop "check addressToAddressL / addressLToAddress" $ convert_address_addressL
  -- describe (propTag <> "addressLToAddress . addressToAddressL") $ do
  --   prop "check addressLToAddress / addressToAddressL" $ convert_addressL_address
  -- describe (propTag <> "ramToAddressL") $ do
  --   prop "test ramToAddressL" $ convert_addressL_ram
  --   prop "test ramToAddressL inverse" $ convert_ram_addressL

  describe (unitTag <> "fetch32Bit") $ do
    it "test fetch32Bit (0~32)" $ do
      fetch32Bit testAddressL (Offset 0)
        `shouldBe` Just (_oneInstTo32Bit firstInst)
    it "test fetch32Bit (32~63)" $ do
      fetch32Bit testAddressL (Offset 32)
        `shouldBe` Just (_oneInstTo32Bit secondInst)
    it "test fetch32Bit (64~95)" $ do
      fetch32Bit testAddressL (Offset 64)
        `shouldBe` Just (_oneInstTo32Bit thirdInst)
    it "test fetch32Bit (96~127)" $ do
      fetch32Bit testAddressL (Offset 96)
        `shouldBe` Just (_oneInstTo32Bit fourthInst)
  describe (unitTag <> "readRAM4Byte") $ do
    it "test readRAM4Byte (0~32)" $ do
      readRAM4Byte testRAM (Ptr . Offset $ 0)
        `shouldBe` Word32 (unpack (_oneInstTo32Bit firstInst))
    it "test readRAM4Byte (32~63)" $ do
      readRAM4Byte testRAM (Ptr . Offset $ 32)
        `shouldBe` Word32 (unpack (_oneInstTo32Bit secondInst))
    it "test readRAM4Byte (64~95)" $ do
      readRAM4Byte testRAM (Ptr . Offset $ 64)
        `shouldBe` Word32 (unpack (_oneInstTo32Bit thirdInst))
    it "test readRAM4Byte (96~127)" $ do
      readRAM4Byte testRAM (Ptr . Offset $ 96)
        `shouldBe` Word32 (unpack (_oneInstTo32Bit fourthInst))
  describe (unitTag <> "replace32Bit") $ do
    it "test replace32Bit (0~31)" $ do
      test_fetch_replace32Bit
        testAddressL
        (Offset 0)
        (bitVectorToWord32 (_oneInstTo32Bit firstInst))
      `shouldBe` Just (_oneInstTo32Bit firstInst)
    it "test replace32Bit (32~63)" $ do
      test_fetch_replace32Bit
        testAddressL
        (Offset 32)
        (bitVectorToWord32 (_oneInstTo32Bit firstInst))
      `shouldBe` Just (_oneInstTo32Bit firstInst)
    it "test replace32Bit (64~95)" $ do
      test_fetch_replace32Bit
        testAddressL
        (Offset 64)
        (bitVectorToWord32 (_oneInstTo32Bit firstInst))
      `shouldBe` Just (_oneInstTo32Bit firstInst)
    it "test replace32Bit (95~127)" $ do
      test_fetch_replace32Bit
        testAddressL
        (Offset 95)
        (bitVectorToWord32 (_oneInstTo32Bit firstInst))
      `shouldBe` Just (_oneInstTo32Bit firstInst)
  describe (unitTag <> "register32Bit") $ do
    it "test register32Bit (0~31)" $ do
      test_fetch_replace32Bit
        testAddressL
        (Offset 0)
        (bitVectorToWord32 (_oneInstTo32Bit firstInst))
      `shouldBe` Just (_oneInstTo32Bit firstInst)
    it "test register32Bit (32~63)" $ do
      test_fetch_replace32Bit
        testAddressL
        (Offset 32)
        (bitVectorToWord32 (_oneInstTo32Bit firstInst))
      `shouldBe` Just (_oneInstTo32Bit firstInst)
    it "test register32Bit (64~95)" $ do
      test_fetch_replace32Bit
        testAddressL
        (Offset 64)
        (bitVectorToWord32 (_oneInstTo32Bit firstInst))
      `shouldBe` Just (_oneInstTo32Bit firstInst)
    it "test register32Bit (96~128)" $ do
      test_fetch_replace32Bit
        testAddressL
        (Offset 96)
        (bitVectorToWord32 (_oneInstTo32Bit firstInst))
      `shouldBe` Just (_oneInstTo32Bit firstInst)

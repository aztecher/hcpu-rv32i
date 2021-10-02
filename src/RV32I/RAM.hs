{-# LANGUAGE NoImplicitPrelude #-}
module RV32I.RAM where

import RV32I.Word
import RV32I.Instruction
import Clash.Prelude hiding (fromList, (!!), length, take, drop)
import Clash.Sized.Signed (Signed)
import Clash.Class.BitPack (pack, unpack)
import Clash.Class.Resize (resize)
import Clash.Sized.BitVector
import Clash.Sized.Vector (fromList, concatBitVector#)
import Clash.XException (NFDataX)
import GHC.Generics (Generic)
import Prelude
  (Show, Eq, (+), (-), ($), fmap, length, (!!), take, drop)
import GHC.TypeLits


-- RAM
-- data RAM = RAM (Vec 3000 Word32) deriving (Show, Eq, Generic, NFDataX)
data RAM = RAM (Vec 64 Word32) deriving (Show, Eq, Generic, NFDataX)
-- newtype Ptr = Ptr (Signed 32) deriving (Show, Eq, Generic, NFDataX)
newtype Ptr = Ptr Offset deriving (Show, Eq, Generic, NFDataX)

data Address = Address (Vec 2048 (BitVector 1)) deriving (Show, Eq, Generic, NFDataX)
type AddressL = [BitVector 1]
data Offset  = Offset Int deriving (Show, Read, Eq, Generic, NFDataX)

-- Int -> Signed 32 of Offset value
-- TODO: we want to convert Offset Int to Offset (Signed 32)
--       and AddressL to Vec n (BitVector 1)
--       and rewrite some helper function
offsetToSigned32 :: Offset -> Signed 32
offsetToSigned32 (Offset val) = fromIntegral . toInteger $ val

signed32ToOffset :: Signed 32 -> Offset
signed32ToOffset signed = Offset (fromIntegral . toInteger $ signed)

readRAM :: RAM -> Ptr -> Word32
readRAM = readRAM4Byte

readRAM4Byte :: RAM -> Ptr -> Word32
readRAM4Byte ram (Ptr offset) = case fetch32Bit (ramToAddressL ram) offset of
    Just bytes -> Word32 (unpack bytes :: Signed 32)
    Nothing -> error $ "Error while reading 4Byte RAM"

readRAM2Byte :: RAM -> Ptr -> Word16
readRAM2Byte ram (Ptr offset) = case fetch16Bit (ramToAddressL ram) offset of
    Just bytes -> Word16 (unpack bytes :: Signed 16)
    Nothing -> error $ "Error while reading 2Byte RAM"

readRAM1Byte :: RAM -> Ptr -> Word8
readRAM1Byte ram (Ptr offset) = case fetch8Bit (ramToAddressL ram) offset of
    Just bytes -> Word8 (unpack bytes :: Signed 8)
    Nothing -> error $ "Error while reading 1Byte RAM"

writeRAM :: RAM -> Ptr -> Word32 -> RAM
writeRAM = writeRAM4Byte

writeRAM4Byte :: RAM -> Ptr -> Word32 -> RAM
writeRAM4Byte ram (Ptr offset) word32 = case register32Bit ram offset word32 of
  Just address -> addressToRAM address
  Nothing -> error $ "Error while writing 4Byte to RAM"

writeRAM2Byte :: RAM -> Ptr -> Word16 -> RAM
writeRAM2Byte ram (Ptr offset) word16 = case register16Bit ram offset word16 of
  Just address -> addressToRAM address
  Nothing -> error $ "Error while writing 2Byte to RAM"

writeRAM1Byte :: RAM -> Ptr -> Word8 -> RAM
writeRAM1Byte ram (Ptr offset) word8 = case register8Bit ram offset word8 of
  Just address -> addressToRAM address
  Nothing -> error $ "Error while writing 1Byte to RAM"

increment :: Ptr -> Ptr
increment (Ptr (Offset offset)) = Ptr (Offset $ offset + 32)

addptr :: Ptr -> Signed 32 -> Ptr
addptr (Ptr (Offset offset)) x = Ptr (Offset $ offset + (fromIntegral . toInteger $ x))

programmedRAM :: (KnownNat n, KnownNat m, (n + m) ~ 64) => Vec n Instruction -> RAM
programmedRAM inst = RAM $ paddingEncodedInstruction (fmap encodeInstruction inst) (repeat (Word32 0))

paddingEncodedInstruction ::
  forall n m . (KnownNat n, KnownNat m, (n + m) ~ 64)
    => Vec n Word32
    -> Vec m Word32
    -> Vec 64 Word32
paddingEncodedInstruction inst zeroInst = inst ++ zeroInst

-- Address
word32ToBitVector :: Word32 -> BitVector 32
word32ToBitVector (Word32 n) = pack n

bitVectorToWord32 :: BitVector 32 -> Word32
bitVectorToWord32 = Word32 . unpack

-- Word32 -> [BitVector 1] (size = 32)
word32ToAddressL :: Word32 -> [BitVector 1]
word32ToAddressL (Word32 signed32) = toList vec32bitvec1
  where
    bitvector32 :: BitVector 32
    bitvector32 = pack signed32
    vec32bitvec1 :: Vec 32 (BitVector 1)
    vec32bitvec1 = unconcatBitVector# bitvector32

word16ToAddressL :: Word16 -> [BitVector 1]
word16ToAddressL (Word16 signed16) = toList vec16bitvec1
  where
    bitvector16 :: BitVector 16
    bitvector16 = pack signed16
    vec16bitvec1 :: Vec 16 (BitVector 1)
    vec16bitvec1 = unconcatBitVector# bitvector16

word8ToAddressL :: Word8 -> [BitVector 1]
word8ToAddressL (Word8 signed8) = toList vec8bitvec1
  where
    bitvector8 :: BitVector 8
    bitvector8 = pack signed8
    vec8bitvec1 :: Vec 8 (BitVector 1)
    vec8bitvec1 = unconcatBitVector# bitvector8


ramToAddress :: RAM -> Address
ramToAddress (RAM ram) = Address $ convert ram
  where
    ramBitVector :: Vec 64 Word32 -> Vec 64 (BitVector 32)
    ramBitVector = fmap word32ToBitVector
    bitVectorToVec :: Vec 64 (BitVector 32) -> Vec 64 (Vec 32 (BitVector 1))
    bitVectorToVec = fmap unconcatBitVector#
    concatVecs :: Vec 64 (Vec 32 (BitVector 1)) -> Vec 2048 (BitVector 1)
    concatVecs = concat
    convert :: Vec 64 Word32 -> Vec 2048 (BitVector 1)
    convert = concatVecs . bitVectorToVec . ramBitVector

addressToRAM :: Address -> RAM
addressToRAM (Address addr) =
  RAM $ fmap bitVectorToWord32 (unconcatBitVector# . concatBitVector# $ addr)

addressToList :: Address -> AddressL
addressToList (Address vector) = toList vector

ramToAddressL :: RAM -> AddressL
ramToAddressL = addressToList . ramToAddress

-- Reuire it's length is 320000
listToAddress :: AddressL -> Maybe Address
listToAddress list = case (fromList list :: Maybe (Vec 2048 (BitVector 1))) of
                          Just v  -> Just (Address v)
                          Nothing -> error $ "Cannot convert AddressL to Address"

fetch32Bit :: AddressL -> Offset -> Maybe (BitVector 32)
fetch32Bit list offset = _fetchSizedBit list offset 31

fetch16Bit :: AddressL -> Offset -> Maybe (BitVector 16)
fetch16Bit list offset = _fetchSizedBit list offset 15

fetch8Bit :: AddressL -> Offset -> Maybe (BitVector 8)
fetch8Bit list offset = _fetchSizedBit list offset 7

_fetchSizedBit :: KnownNat n => AddressL -> Offset -> Int -> Maybe (BitVector n)
_fetchSizedBit list offset n = do
  list <- _slice list offset n
  vec  <- fromList list
  return $ concatBitVector# vec

register32Bit :: RAM -> Offset -> Word32 -> Maybe Address
register32Bit ram offset word32
  = listToAddress $ replace32Bit (ramToAddressL ram) offset word32

replace32Bit :: AddressL -> Offset -> Word32 -> AddressL
replace32Bit address offset word32 = _replaceSizedBit address offset replacebits 32
  where
    replacebits = word32ToAddressL word32

register16Bit :: RAM -> Offset -> Word16 -> Maybe Address
register16Bit ram offset word16
  = listToAddress $ replace16Bit (ramToAddressL ram) offset word16

replace16Bit :: AddressL -> Offset -> Word16 -> AddressL
replace16Bit address offset word16 = _replaceSizedBit address offset replacebits 16
  where
    replacebits = word16ToAddressL word16

register8Bit :: RAM -> Offset -> Word8 -> Maybe Address
register8Bit ram offset word8
  = listToAddress $ replace8Bit (ramToAddressL ram) offset word8

replace8Bit :: AddressL -> Offset -> Word8 -> AddressL
replace8Bit address offset word8 = _replaceSizedBit address offset replacebits 8
  where
    replacebits = word8ToAddressL word8

_replaceSizedBit :: AddressL -> Offset -> AddressL -> Int -> AddressL
_replaceSizedBit address (Offset offset) replacebits n
  = before <> replacebits <> after
  where
    before = take offset address
    after  = drop (offset + n) address

_fetch32Bit :: AddressL -> Offset -> Maybe AddressL
_fetch32Bit bv offset = _slice bv offset 31

_slice :: AddressL -> Offset -> Int -> Maybe AddressL
_slice orig (Offset offset) size = case (length orig) - 1 <= offset + size of
  True -> error $ "Error while slicing address space"
  False -> Just (slice' offset (offset + size) orig)
  where
    slice' from to xs = take (to - from + 1) (drop from xs)


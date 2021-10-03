{-# LANGUAGE NoImplicitPrelude #-}
module RV32I.RAM where

import RV32I.Word
import RV32I.Instruction
import Clash.Prelude hiding (fromList, length, take, drop)
import Clash.Sized.Signed (Signed)
import Clash.Class.BitPack (pack, unpack)
import Clash.Class.Resize (resize)
import Clash.Sized.BitVector
import Clash.Sized.Vector (fromList, replace, concatBitVector#)
import Clash.XException (NFDataX)
import GHC.Generics (Generic)
import Prelude
  (Show, Eq, (+), (-), ($), fmap, length, take, drop)
import GHC.TypeLits


-- RAM
data RAM = RAM (Vec 2048 Word8) deriving (Show, Eq, Generic, NFDataX)
newtype Ptr = Ptr (Signed 32) deriving (Show, Eq, Generic, NFDataX)

readRAM :: RAM -> Ptr -> Word32
readRAM = readRAM4Byte

readRAM4Byte :: RAM -> Ptr -> Word32
readRAM4Byte (RAM contents) (Ptr address) = bytes
  where
    bit0To7 = contents !! (address + 3)
    bit8To15 = contents !! (address + 2)
    bit16To23 = contents !! (address + 1)
    bit24To31 = contents !! address
    bytes = concatWord8ToWord32 bit24To31 bit16To23 bit8To15 bit0To7

readRAM2Byte :: RAM -> Ptr -> Word16
readRAM2Byte (RAM contents) (Ptr address) = bytes
  where
    bit0To7 = contents !! (address + 1)
    bit8To15 = contents !! address
    bytes = concatWord8ToWord16 bit8To15 bit0To7

readRAM1Byte :: RAM -> Ptr -> Word8
readRAM1Byte (RAM contents) (Ptr address) = contents !! address

writeRAM :: RAM -> Ptr -> Word32 -> RAM
writeRAM = writeRAM4Byte

writeRAM4Byte :: RAM -> Ptr -> Word32 -> RAM
writeRAM4Byte (RAM contents) (Ptr address) word32 = RAM contents''''
  where
    (bv32To24, bv23To16, bv15To8, bv7To0) = separateWord32ToWord8 word32
    contents'    = replace address       bv32To24 contents
    contents''   = replace (address + 1) bv23To16 contents'
    contents'''  = replace (address + 2) bv15To8  contents''
    contents'''' = replace (address + 3) bv7To0   contents'''

writeRAM2Byte :: RAM -> Ptr -> Word16 -> RAM
writeRAM2Byte (RAM contents) (Ptr address) word16 = RAM contents''
  where
    (bv15To8, bv7To0) = separateWord16ToWord8 word16
    contents'  = replace address       bv15To8 contents
    contents'' = replace (address + 1) bv7To0  contents'


writeRAM1Byte :: RAM -> Ptr -> Word8 -> RAM
writeRAM1Byte (RAM contents) (Ptr address) word8 = RAM (replace address word8 contents)

increment :: Ptr -> Ptr
increment (Ptr address) = Ptr (address + 4)

addptr :: Ptr -> Signed 32 -> Ptr
addptr (Ptr address) offset = Ptr (address + offset)


programmedRAM inst = ram
  where
    instSpace = paddingEncodedInstruction (fmap encodeInstruction inst) (repeat (Word32 0))
    ram = RAM . convertVector $ instSpace

convertVector :: Vec 512 Word32 -> Vec 2048 Word8
convertVector = toVec . toBv
  where
    vecWordToBv :: Vec 512 Word32 -> Vec 512 (BitVector 32)
    vecWordToBv = fmap word32ToBitVector
    toBv :: Vec 512 Word32 -> BitVector 16384
    toBv = concatBitVector# . vecWordToBv
    bvToVec :: BitVector 16384 -> Vec 2048 (BitVector 8)
    bvToVec = unconcatBitVector#
    toVec :: BitVector 16384 -> Vec 2048 Word8
    toVec bv = fmap bitVectorToWord8 (bvToVec bv)

paddingEncodedInstruction ::
  forall n m . (KnownNat n, KnownNat m, (n + m) ~ 512)
    => Vec n Word32
    -> Vec m Word32
    -> Vec 512 Word32
paddingEncodedInstruction inst zeroInst = inst ++ zeroInst

word8ToBitVector :: Word8 -> BitVector 8
word8ToBitVector (Word8 n) = pack n

bitVectorToWord8 :: BitVector 8 -> Word8
bitVectorToWord8 = Word8 . unpack

word32ToBitVector :: Word32 -> BitVector 32
word32ToBitVector (Word32 n) = pack n

bitVectorToWord32 :: BitVector 32 -> Word32
bitVectorToWord32 = Word32 . unpack

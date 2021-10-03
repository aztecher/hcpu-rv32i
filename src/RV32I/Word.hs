module RV32I.Word where

import Clash.Prelude (slice)
import Clash.Sized.Signed (Signed)
import Clash.Class.BitPack (pack, unpack)
import Clash.Sized.BitVector ((++#), BitVector)
import Clash.Promoted.Nat.Literals as Nat
import Clash.XException (NFDataX)
import GHC.Generics (Generic)

-- WordX is represents X bit that will intepreted as unsined.
newtype Word32 = Word32 (Signed 32) deriving (Show, Eq, Generic, NFDataX)
newtype Word20 = Word20 (Signed 20) deriving (Show, Eq, Generic, NFDataX)
newtype Word16 = Word16 (Signed 16) deriving (Show, Eq, Generic, NFDataX)
newtype Word12 = Word12 (Signed 12) deriving (Show, Eq, Generic, NFDataX)
newtype Word8  = Word8  (Signed  8) deriving (Show, Eq, Generic, NFDataX)
newtype Word7  = Word7  (Signed  7) deriving (Show, Eq, Generic, NFDataX)
newtype Word5  = Word5  (Signed  5) deriving (Show, Eq, Generic, NFDataX)
newtype Word4  = Word4  (Signed  4) deriving (Show, Eq, Generic, NFDataX)


concatWord8ToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
concatWord8ToWord32 (Word8 n1) (Word8 n2) (Word8 n3) (Word8 n4) = Word32 n
  where
    bvs = (pack n1) ++# (pack n2) ++# (pack n3) ++# (pack n4)
    n = unpack bvs

concatWord8ToWord16 :: Word8 -> Word8 -> Word16
concatWord8ToWord16 (Word8 n1) (Word8 n2) = Word16 n
  where
    bvs = (pack n1) ++# (pack n2)
    n = unpack bvs

separateWord32ToWord8 :: Word32 -> (Word8, Word8, Word8, Word8)
separateWord32ToWord8 (Word32 n32) = (bv32To24, bv23To16, bv15To8, bv7To0)
  where
    bv = pack n32
    bv32To24 = Word8 $ unpack (slice Nat.d31 Nat.d24 bv)
    bv23To16 = Word8 $ unpack (slice Nat.d23 Nat.d16 bv)
    bv15To8  = Word8 $ unpack (slice Nat.d15 Nat.d8  bv)
    bv7To0   = Word8 $ unpack (slice Nat.d7  Nat.d0  bv)

separateWord16ToWord8 :: Word16 -> (Word8, Word8)
separateWord16ToWord8 (Word16 n16) = (bv15To8, bv7To0)
  where
    bv = pack n16
    bv15To8 = Word8 $ unpack (slice Nat.d15 Nat.d8 bv)
    bv7To0  = Word8 $ unpack (slice Nat.d7  Nat.d0 bv)

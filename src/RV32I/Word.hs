module RV32I.Word where

import Clash.Sized.Signed (Signed)
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


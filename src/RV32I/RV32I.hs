{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
module RV32I.RV32I where

import Clash.Prelude (slice, mapM_, print)
import Clash.Promoted.Nat.Literals as Nat
import Clash.Sized.BitVector (BitVector, Bit, (++#), size#)
import Clash.Sized.Internal.BitVector
  (and#, or#, xor#, toInteger#, fromInteger#, shiftL#, shiftR#, index#,
   msb#, replaceBit#, lt#, gt#, ge#, complement#, neq#)
import Clash.Sized.Signed (Signed)
import Clash.Sized.Unsigned (Unsigned)
import Clash.Class.BitPack (pack, unpack)
import Clash.Class.Resize (resize)
import Clash.Sized.Vector (Vec((:>), Nil), (!!), replace, repeat, (++), length, zipWith)
import Clash.Signal (Signal, System, HiddenClockResetEnable, register, sample, exposeClockResetEnable)
import Clash.Signal.Internal (Domain, Clock, Reset, Enable)
import Clash.Explicit.Signal (systemClockGen, systemResetGen)
import Clash.Signal.Internal (enableGen)
import Clash.XException (NFDataX)
import GHC.Generics (Generic)
import GHC.TypeLits

-- Debug
import GHC.Stack (HasCallStack)

import Prelude ((+), (-), ($), (==), (>=), (<),
  otherwise, undefined, fromIntegral, fmap, id, take, print,
  Show, Eq, Enum, IO, Int, Functor, Integer, Bool(..), Maybe(..))
import Data.Function (fix)

-- WordX is represents X bit that will intepreted as unsined.
newtype Word32 = Word32 (Signed 32) deriving (Show, Eq, Generic, NFDataX)
newtype Word20 = Word20 (Signed 20) deriving (Show, Eq, Generic, NFDataX)
newtype Word16 = Word16 (Signed 16) deriving (Show, Eq, Generic, NFDataX)
newtype Word12 = Word12 (Signed 12) deriving (Show, Eq, Generic, NFDataX)
newtype Word8  = Word8  (Signed  8) deriving (Show, Eq, Generic, NFDataX)
newtype Word7  = Word7  (Signed  7) deriving (Show, Eq, Generic, NFDataX)
newtype Word5  = Word5  (Signed  5) deriving (Show, Eq, Generic, NFDataX)
newtype Word4  = Word4  (Signed  4) deriving (Show, Eq, Generic, NFDataX)


-- RAM
data RAM = RAM (Vec 1000 Word32) deriving (Show, Generic, NFDataX)

data Register
  = Zero -- constant value : 0
  | RA   -- return address
  | SP   -- stack pointer
  | GP   -- global pointer
  | TP   -- thread pointer
  | T0   -- temporary register
  | T1
  | T2
  | S0   -- save register / frame pointer
  | S1   -- save register
  | A0   -- argument, return value of function
  | A1
  | A2   -- argument value of function
  | A3
  | A4
  | A5
  | A6
  | A7
  | S2   -- save register
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | T3   -- temporary register
  | T4
  | T5
  | T6
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- Opcode
-- [ R | I | S | B | U | J ](Opcode name for distinguishing format)

-- R Opcode
data RArith
  = ADD
  | SUB
  | AND
  | OR
  | XOR
  | SLL
  | SRA
  | SRL
  | SLT
  | SLTU
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- I Opcode
data IArith
  = ADDI
  | SLTI
  | SLTIU
  | XORI
  | ORI
  | ANDI
  deriving (Show, Generic, NFDataX, Eq, Enum)

data IShift
  = SLLI
  | SRAI
  | SRLI
  deriving (Show, Generic, NFDataX, Eq, Enum)

data IJalr = JALR deriving (Show, Generic, NFDataX, Eq, Enum)

data ILoad
  = LB
  | LH
  | LW
  | LBU
  | LHU
  deriving (Show, Generic, NFDataX, Eq, Enum)

data IFence = FENCE deriving (Show, Generic, NFDataX, Eq, Enum)

data IFencei = FENCEI deriving (Show, Generic, NFDataX, Eq, Enum)

data IEnv
  = EBREAK
  | ECALL
  deriving (Show, Generic, NFDataX, Eq, Enum)

data ICsr
  = CSRRC
  | CSRRS
  | CSRRW
  deriving (Show, Generic, NFDataX, Eq, Enum)

data ICsri
  = CSRRCI
  | CSRRSI
  | CSRRWI
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- S Opcode
data SStore
  = SB
  | SH
  | SW
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- B Opcode
data BBranch
  = BEQ
  | BNE
  | BLT
  | BGE
  | BLTU
  | BGEU
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- U Opcode
data UArith
  = LUI
  | AUIPC
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- J Opcode
data JJal = JAL deriving (Show, Generic, NFDataX, Eq, Enum)


-- R Format
data RFormat
  = RArith RArith
           Register
           Register
           Register
  deriving (Show, Generic, NFDataX, Eq)

-- I Format
data IFormat
  = IArith  IArith
            Word12
            Register
            Register
  | IShift  IShift
            Word5
            Register
            Register
  | IJalr   IJalr
            Word12
            Register
            Register
  | ILoad   ILoad
            Word12
            Register
            Register
  | IFence  IFence
            Word4
            Word4
  | IFencei IFencei
  | IEnv    IEnv
  | ICsr    ICsr
            Word12
            Register
            Register
  | ICsri   ICsri
            Word12
            Word5
            Register
  deriving (Show, Generic, NFDataX, Eq)

-- S Format
data SFormat
  = SStore SStore
           Word7
           Register
           Register
           Word5
  deriving (Show, Generic, NFDataX, Eq)

-- B Format
data BFormat
  = BBranch BBranch
            Word7
            Register
            Register
            Word5
  deriving (Show, Generic, NFDataX, Eq)

-- U Format
data UFormat
  = UArith UArith
           Word20
           Register
  deriving (Show, Generic, NFDataX, Eq)

-- J Format
data JFormat
  = JJal JJal
         Word20
         Register
  deriving (Show, Generic, NFDataX, Eq)

-- RV32I instruction
data Instruction
  = R RFormat
  | I IFormat
  | S SFormat
  | B BFormat
  | U UFormat
  | J JFormat
  deriving (Show, Generic, NFDataX, Eq)


-- CPU Registers
data Registers = Registers {
  zero :: Signed 32,
  ra   :: Signed 32,
  sp   :: Signed 32,
  gp   :: Signed 32,
  tp   :: Signed 32,
  t0   :: Signed 32,
  t1   :: Signed 32,
  t2   :: Signed 32,
  s0fp :: Signed 32,
  s1   :: Signed 32,
  a0   :: Signed 32,
  a1   :: Signed 32,
  a2   :: Signed 32,
  a3   :: Signed 32,
  a4   :: Signed 32,
  a5   :: Signed 32,
  a6   :: Signed 32,
  a7   :: Signed 32,
  s2   :: Signed 32,
  s3   :: Signed 32,
  s4   :: Signed 32,
  s5   :: Signed 32,
  s6   :: Signed 32,
  s7   :: Signed 32,
  s8   :: Signed 32,
  s9   :: Signed 32,
  s10  :: Signed 32,
  s11  :: Signed 32,
  t3   :: Signed 32,
  t4   :: Signed 32,
  t5   :: Signed 32,
  t6   :: Signed 32,
  pc   :: Ptr
} deriving (Show, Generic, NFDataX, Eq)


encodeRegister :: Register -> BitVector 5
encodeRegister Zero = 0
encodeRegister RA = 1
encodeRegister SP = 2
encodeRegister GP = 3
encodeRegister TP = 4
encodeRegister T0 = 5
encodeRegister T1 = 6
encodeRegister T2 = 7
encodeRegister S0 = 8
encodeRegister S1 = 9
encodeRegister A0 = 10
encodeRegister A1 = 11
encodeRegister A2 = 12
encodeRegister A3 = 13
encodeRegister A4 = 14
encodeRegister A5 = 15
encodeRegister A6 = 16
encodeRegister A7 = 17
encodeRegister S2 = 18
encodeRegister S3 = 19
encodeRegister S4 = 20
encodeRegister S5 = 21
encodeRegister S6 = 22
encodeRegister S7 = 23
encodeRegister S8 = 24
encodeRegister S9 = 25
encodeRegister S10 = 26
encodeRegister S11 = 27
encodeRegister T3 = 28
encodeRegister T4 = 29
encodeRegister T5 = 30
encodeRegister T6 = 31

encodeInstruction :: Instruction -> Word32
encodeInstruction instr = case instr of
  R format -> rEncode format
  I format -> iEncode format
  S format -> sEncode format
  B format -> bEncode format
  U format -> uEncode format
  J format -> jEncode format
  where
    rEncode :: RFormat -> Word32
    rEncode (RArith arith rs2 rs1 rd) = Word32 $ unpack $ case arith of
      ADD  -> funct7 0  ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 0 ++# encodeRegister rd ++# opcode 51
      SUB  -> funct7 32 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 0 ++# encodeRegister rd ++# opcode 51
      AND  -> funct7 0  ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 7 ++# encodeRegister rd ++# opcode 51
      OR   -> funct7 0  ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 6 ++# encodeRegister rd ++# opcode 51
      XOR  -> funct7 0  ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 4 ++# encodeRegister rd ++# opcode 51
      SLL  -> funct7 0  ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 1 ++# encodeRegister rd ++# opcode 51
      SRA  -> funct7 32 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 5 ++# encodeRegister rd ++# opcode 51
      SRL  -> funct7 0  ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 5 ++# encodeRegister rd ++# opcode 51
      SLT  -> funct7 0  ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 2 ++# encodeRegister rd ++# opcode 51
      SLTU -> funct7 0  ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 3 ++# encodeRegister rd ++# opcode 51
    iEncode :: IFormat -> Word32
    iEncode (IArith  arith (Word12 im12) rs1 rd) = Word32 $ unpack $ case arith of
      ADDI  -> pack im12 ++# encodeRegister rs1 ++# funct3 0 ++# encodeRegister rd ++# opcode 19
      SLTI  -> pack im12 ++# encodeRegister rs1 ++# funct3 2 ++# encodeRegister rd ++# opcode 19
      SLTIU -> pack im12 ++# encodeRegister rs1 ++# funct3 3 ++# encodeRegister rd ++# opcode 19
      XORI  -> pack im12 ++# encodeRegister rs1 ++# funct3 4 ++# encodeRegister rd ++# opcode 19
      ORI   -> pack im12 ++# encodeRegister rs1 ++# funct3 6 ++# encodeRegister rd ++# opcode 19
      ANDI  -> pack im12 ++# encodeRegister rs1 ++# funct3 7 ++# encodeRegister rd ++# opcode 19
    iEncode (IShift  shift (Word5 shamt) rs1 rd) = Word32 $ unpack $ case shift of
      SLLI -> funct7 0  ++# pack shamt ++# encodeRegister rs1 ++# funct3 1 ++# encodeRegister rd ++# opcode 19
      SRAI -> funct7 32 ++# pack shamt ++# encodeRegister rs1 ++# funct3 5 ++# encodeRegister rd ++# opcode 19
      SRLI -> funct7 0  ++# pack shamt ++# encodeRegister rs1 ++# funct3 5 ++# encodeRegister rd ++# opcode 19
    iEncode (IJalr   jalr  (Word12 offset) rs1 rd) = Word32 $ unpack $ case jalr of
      JALR -> pack offset ++# encodeRegister rs1 ++# funct3 0 ++# encodeRegister rd ++# opcode 103
    iEncode (ILoad   load  (Word12 offset) rs1 rd) = Word32 $ unpack $ case load of
      LB   -> pack offset ++# encodeRegister rs1 ++# funct3 0 ++# encodeRegister rd ++# opcode 3
      LH   -> pack offset ++# encodeRegister rs1 ++# funct3 1 ++# encodeRegister rd ++# opcode 3
      LW   -> pack offset ++# encodeRegister rs1 ++# funct3 2 ++# encodeRegister rd ++# opcode 3
      LBU  -> pack offset ++# encodeRegister rs1 ++# funct3 4 ++# encodeRegister rd ++# opcode 3
      LHU  -> pack offset ++# encodeRegister rs1 ++# funct3 5 ++# encodeRegister rd ++# opcode 3
    iEncode (IFence  fence (Word4 pred) (Word4 succ)) = Word32 $ unpack $ case fence of
      FENCE -> const4 0 ++# pack pred ++# pack succ ++# const5 0 ++# funct3 0 ++# const5 0 ++# opcode 15
    iEncode (IFencei fencei) = Word32 $ unpack $ case fencei of
      FENCEI -> const12 0 ++# const5 0 ++# funct3 1 ++# const5 0 ++# opcode 15
    iEncode (IEnv    env) = Word32 $ unpack $ case env of
      EBREAK -> const12 1 ++# const5 0 ++# funct3 0 ++# const5 0 ++# opcode 115
      ECALL  -> const12 0 ++# const5 0 ++# funct3 0 ++# const5 0 ++# opcode 115
    iEncode (ICsr    csr (Word12 c) rs1 rd) = Word32 $ unpack $ case csr of
      CSRRC -> pack c ++# encodeRegister rs1 ++# funct3 3 ++# encodeRegister rd ++# opcode 115
      CSRRS -> pack c ++# encodeRegister rs1 ++# funct3 2 ++# encodeRegister rd ++# opcode 115
      CSRRW -> pack c ++# encodeRegister rs1 ++# funct3 1 ++# encodeRegister rd ++# opcode 115
    iEncode (ICsri   csri (Word12 c) (Word5 zimm) rd) = Word32 $ unpack $ case csri of
      CSRRCI -> pack c ++# pack zimm ++# funct3 7 ++# encodeRegister rd ++# opcode 115
      CSRRSI -> pack c ++# pack zimm ++# funct3 6 ++# encodeRegister rd ++# opcode 115
      CSRRWI -> pack c ++# pack zimm ++# funct3 5 ++# encodeRegister rd ++# opcode 115
    sEncode :: SFormat -> Word32
    sEncode (SStore  store (Word7 offset1) rs2 rs1 (Word5 offset2)) = Word32 $ unpack $ case store of
      SB -> pack offset1 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 0 ++# pack offset2 ++# opcode 35
      SH -> pack offset1 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 1 ++# pack offset2 ++# opcode 35
      SW -> pack offset1 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 2 ++# pack offset2 ++# opcode 35
    bEncode :: BFormat -> Word32
    bEncode (BBranch br (Word7 offset1) rs2 rs1 (Word5 offset2)) = Word32 $ unpack $ case br of
      BEQ  -> pack offset1 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 0 ++# pack offset2 ++# opcode 99
      BNE  -> pack offset1 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 1 ++# pack offset2 ++# opcode 99
      BLT  -> pack offset1 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 4 ++# pack offset2 ++# opcode 99
      BGE  -> pack offset1 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 5 ++# pack offset2 ++# opcode 99
      BLTU -> pack offset1 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 6 ++# pack offset2 ++# opcode 99
      BGEU -> pack offset1 ++# encodeRegister rs2 ++# encodeRegister rs1 ++# funct3 7 ++# pack offset2 ++# opcode 99
    uEncode :: UFormat -> Word32
    uEncode (UArith  arith (Word20 im20) rd) = Word32 $ unpack $ case arith of
      LUI   -> pack im20 ++# encodeRegister rd ++# opcode 55
      AUIPC -> pack im20 ++# encodeRegister rd ++# opcode 23
    jEncode :: JFormat -> Word32
    jEncode (JJal    jal (Word20 offset) rd) = Word32 $ unpack $ case jal of
      JAL -> pack offset ++# encodeRegister rd ++# opcode 111
    -- helper
    opcode :: BitVector 7 -> BitVector 7
    opcode x = x
    opcode23 = 23
    funct3 :: BitVector 3 -> BitVector 3
    funct3 x = x
    funct7 :: BitVector 7 -> BitVector 7
    funct7 x = x
    const4 :: BitVector 4 -> BitVector 4
    const4 x = x
    const5 :: BitVector 5 -> BitVector 5
    const5 x = x
    const12 :: BitVector 12 -> BitVector 12
    const12 x = x

decodeRegister :: BitVector 5 -> Register
decodeRegister 0 = Zero
decodeRegister 1 = RA
decodeRegister 2 = SP
decodeRegister 3 = GP
decodeRegister 4 = TP
decodeRegister 5 = T0
decodeRegister 6 = T1
decodeRegister 7 = T2
decodeRegister 8 = S0
decodeRegister 9 = S1
decodeRegister 10 = A0
decodeRegister 11 = A1
decodeRegister 12 = A2
decodeRegister 13 = A3
decodeRegister 14 = A4
decodeRegister 15 = A5
decodeRegister 16 = A6
decodeRegister 17 = A7
decodeRegister 18 = S2
decodeRegister 19 = S3
decodeRegister 20 = S4
decodeRegister 21 = S5
decodeRegister 22 = S6
decodeRegister 23 = S7
decodeRegister 24 = S8
decodeRegister 25 = S9
decodeRegister 26 = S10
decodeRegister 27 = S11
decodeRegister 28 = T3
decodeRegister 29 = T4
decodeRegister 30 = T5
decodeRegister 31 = T6


decodeInstruction :: Word32 -> Instruction
decodeInstruction word@(Word32 val) = case op of
  x@(51  :: BitVector 7) -> R $ decodeRFormat x word
  x@(19  :: BitVector 7) -> I $ decodeIFormat x word
  x@(103 :: BitVector 7) -> I $ decodeIFormat x word
  x@(3   :: BitVector 7) -> I $ decodeIFormat x word
  x@(15  :: BitVector 7) -> I $ decodeIFormat x word
  x@(115 :: BitVector 7) -> I $ decodeIFormat x word
  x@(35  :: BitVector 7) -> S $ decodeSFormat x word
  x@(99  :: BitVector 7) -> B $ decodeBFormat x word
  x@(55  :: BitVector 7) -> U $ decodeUFormat x word
  x@(23  :: BitVector 7) -> U $ decodeUFormat x word
  x@(111 :: BitVector 7) -> J $ decodeJFormat x word
  where
    op = slice Nat.d6 Nat.d0 val
    funct3 = slice Nat.d14 Nat.d12 val
    -- funct7 = slice Nat.d31 Nat.d25 val
    decodeRFormat :: BitVector 7 -> Word32 -> RFormat
    decodeRFormat _ (Word32 val) = RArith opcode rs2 rs1 rd
      where
        opcode = case funct3 of
          (0 :: BitVector 3) -> case funct7 of
            (0  :: BitVector 7) -> ADD
            (32 :: BitVector 7) -> SUB
          (1 :: BitVector 3) -> SLL
          (2 :: BitVector 3) -> SLT
          (3 :: BitVector 3) -> SLTU
          (4 :: BitVector 3) -> XOR
          (5 :: BitVector 3) -> case funct7 of
            (0  :: BitVector 7) -> SRL
            (32 :: BitVector 7) -> SRA
          (6 :: BitVector 3) -> OR
          (7 :: BitVector 3) -> AND
        funct7 = slice Nat.d31 Nat.d25 val
        rs2    = decodeRegister $ slice Nat.d24 Nat.d20 val
        rs1    = decodeRegister $ slice Nat.d19 Nat.d15 val
        funct3 = slice Nat.d14 Nat.d12 val
        rd     = decodeRegister $ slice Nat.d11 Nat.d7  val
    decodeIFormat :: BitVector 7 -> Word32 -> IFormat
    decodeIFormat op (Word32 val) = case op of
      (19 :: BitVector 7) -> case funct3 of
        (0 :: BitVector 3) -> IArith ADDI  (Word12 $ unpack im12)  rs1 rd
        (1 :: BitVector 3) -> IShift SLLI  (Word5  $ unpack shamt) rs1 rd
        (2 :: BitVector 3) -> IArith SLTI  (Word12 $ unpack im12)  rs1 rd
        (3 :: BitVector 3) -> IArith SLTIU (Word12 $ unpack im12)  rs1 rd
        (4 :: BitVector 3) -> IArith XORI  (Word12 $ unpack im12)  rs1 rd
        (5 :: BitVector 3) -> case funct7 of
          (0  :: BitVector 7) -> IShift SRLI (Word5 $ unpack shamt) rs1 rd
          (32 :: BitVector 7) -> IShift SRAI (Word5 $ unpack shamt) rs1 rd
        (6 :: BitVector 3) -> IArith ORI  (Word12 $ unpack im12) rs1 rd
        (7 :: BitVector 3) -> IArith ANDI (Word12 $ unpack im12) rs1 rd
      (103 :: BitVector 7) -> IJalr JALR (Word12 $ unpack im12) rs1 rd
      (3   :: BitVector 7) -> case funct3 of
        (0 :: BitVector 3) -> ILoad LB  (Word12 $ unpack im12) rs1 rd
        (1 :: BitVector 3) -> ILoad LH  (Word12 $ unpack im12) rs1 rd
        (2 :: BitVector 3) -> ILoad LW  (Word12 $ unpack im12) rs1 rd
        (4 :: BitVector 3) -> ILoad LBU (Word12 $ unpack im12) rs1 rd
        (5 :: BitVector 3) -> ILoad LHU (Word12 $ unpack im12) rs1 rd
      (15  :: BitVector 7) -> case funct3 of
        (0 :: BitVector 3) -> IFence FENCE (Word4 $ unpack pred) (Word4 $ unpack succ)
        (1 :: BitVector 3) -> IFencei FENCEI
      (115 :: BitVector 7) -> case funct3 of
        (0 :: BitVector 3) -> case im12 of
          (0 :: BitVector 12) -> IEnv ECALL
          (1 :: BitVector 12) -> IEnv EBREAK
        (1 :: BitVector 3) -> ICsr CSRRW   (Word12 $ unpack im12) rs1 rd
        (2 :: BitVector 3) -> ICsr CSRRS   (Word12 $ unpack im12) rs1 rd
        (3 :: BitVector 3) -> ICsr CSRRC   (Word12 $ unpack im12) rs1 rd
        (5 :: BitVector 3) -> ICsri CSRRWI (Word12 $ unpack im12) (Word5 $ unpack zimm) rd
        (6 :: BitVector 3) -> ICsri CSRRSI (Word12 $ unpack im12) (Word5 $ unpack zimm) rd
        (7 :: BitVector 3) -> ICsri CSRRCI (Word12 $ unpack im12) (Word5 $ unpack zimm) rd
      where
        im12   = slice Nat.d31 Nat.d20 val
        funct7 = slice Nat.d31 Nat.d25 val
        shamt  = slice Nat.d24 Nat.d20 val
        pred   = slice Nat.d27 Nat.d24 val
        succ   = slice Nat.d23 Nat.d20 val
        zimm   = slice Nat.d19 Nat.d15 val
        rs1    = decodeRegister $ slice Nat.d19 Nat.d15 val
        funct3 = slice Nat.d14 Nat.d12 val
        rd     = decodeRegister $ slice Nat.d11 Nat.d7 val
    decodeSFormat :: BitVector 7 -> Word32 -> SFormat
    decodeSFormat _ (Word32 val) = SStore opcode (Word7 $ unpack offset1) rs2 rs1 (Word5 $ unpack offset2)
      where
        opcode = case funct3 of
          (0 :: BitVector 3) -> SB
          (1 :: BitVector 3) -> SH
          (2 :: BitVector 3) -> SW
        offset1 = slice Nat.d31 Nat.d25 val
        rs2     = decodeRegister $ slice Nat.d24 Nat.d20 val
        rs1     = decodeRegister $ slice Nat.d19 Nat.d15 val
        funct3  = slice Nat.d14 Nat.d12 val
        offset2 = slice Nat.d11 Nat.d7  val
    decodeBFormat :: BitVector 7 -> Word32 -> BFormat
    decodeBFormat _ (Word32 val) = BBranch opcode (Word7 $ unpack offset1) rs2 rs1 (Word5 $ unpack offset2)
      where
        opcode = case funct3 of
          (0 :: BitVector 3) -> BEQ
          (1 :: BitVector 3) -> BNE
          (4 :: BitVector 3) -> BLT
          (5 :: BitVector 3) -> BGE
          (6 :: BitVector 3) -> BLTU
          (7 :: BitVector 3) -> BGEU
        offset1 = slice Nat.d31 Nat.d25 val
        rs2     = decodeRegister $ slice Nat.d24 Nat.d20 val
        rs1     = decodeRegister $ slice Nat.d19 Nat.d15 val
        funct3  = slice Nat.d14 Nat.d12 val
        offset2 = slice Nat.d11 Nat.d7  val
    decodeUFormat :: BitVector 7 -> Word32 -> UFormat
    decodeUFormat op (Word32 val) = case op of
      (55 :: BitVector 7) -> UArith LUI   (Word20 $ unpack im20) rd
      (23 :: BitVector 7) -> UArith AUIPC (Word20 $ unpack im20) rd
      where
        im20 = slice Nat.d31 Nat.d12 val
        rd   = decodeRegister $ slice Nat.d11 Nat.d7  val
    decodeJFormat :: BitVector 7 -> Word32 -> JFormat
    decodeJFormat _ (Word32 val) = JJal JAL (Word20 $ unpack offset) rd
      where
        offset = slice Nat.d31 Nat.d12 val
        rd     = decodeRegister $ slice Nat.d11 Nat.d7  val


newtype Ptr = Ptr (Signed 32) deriving (Show, Eq, Generic, NFDataX)
newtype Output = Output (Signed 32) deriving (Show, Eq, Generic, NFDataX)

readRAM :: RAM -> Ptr -> Word32
readRAM = readRAM4Byte

readRAM4Byte :: RAM -> Ptr -> Word32
readRAM4Byte (RAM contents) (Ptr address) = contents !! address

readRAM2Byte :: RAM -> Ptr -> Word16
readRAM2Byte (RAM contents) (Ptr address) = value
  where
    Word32 value32 = contents !! address
    resized = resize $ pack value32 :: BitVector 16
    value   = Word16 $ unpack resized

readRAM1Byte :: RAM -> Ptr -> Word8
readRAM1Byte (RAM contents) (Ptr address) = value
  where
    Word32 value32 = contents !! address
    resized = resize $ pack value32 :: BitVector 8
    value   = Word8 $ unpack resized

writeRAM :: RAM -> Ptr -> Word32 -> RAM
writeRAM = writeRAM4Byte

writeRAM4Byte :: RAM -> Ptr -> Word32 -> RAM
writeRAM4Byte (RAM contents) (Ptr address) val = RAM (replace address val contents)

writeRAM2Byte :: RAM -> Ptr -> Word16 -> RAM
writeRAM2Byte (RAM contents) (Ptr address) (Word16 val) = value
  where
    resized = resize $ pack val
    value   = RAM (replace address (Word32 $ unpack resized) contents)

writeRAM1Byte :: RAM -> Ptr -> Word8 -> RAM
writeRAM1Byte (RAM contents) (Ptr address) (Word8 val) = value
  where
    resized = resize $ pack val
    value   = RAM (replace address (Word32 $ unpack resized) contents)

increment :: Ptr -> Ptr
increment (Ptr address) = Ptr (address + 1)

addptr :: Ptr -> Signed 32 -> Ptr
addptr (Ptr address) offset = Ptr (address + offset)

readRegister :: Registers -> Register -> Signed 32
readRegister (Registers x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 _) reg = case reg of
  Zero -> x0
  RA -> x1
  SP -> x2
  GP-> x3
  TP -> x4
  T0 -> x5
  T1 -> x6
  T2 -> x7
  S0 -> x8
  S1 -> x9
  A0 -> x10
  A1 -> x11
  A2 -> x12
  A3 -> x13
  A4 -> x14
  A5 -> x15
  A6 -> x16
  A7 -> x17
  S2 -> x18
  S3 -> x19
  S4 -> x20
  S5 -> x21
  S6 -> x22
  S7 -> x23
  S8 -> x24
  S9 -> x25
  S10 -> x26
  S11 -> x27
  T3 -> x28
  T4 -> x29
  T5 -> x30
  T6 -> x31

writeRegister :: Registers -> Register -> Signed 32 -> Registers
writeRegister regs reg word = case reg of
  Zero -> regs {zero = word}
  RA   -> regs {ra = word}
  SP   -> regs {sp = word}
  GP   -> regs {gp = word}
  TP   -> regs {tp = word}
  T0   -> regs {t0 = word}
  T1   -> regs {t1 = word}
  T2   -> regs {t2 = word}
  S0 -> regs {s0fp = word}
  S1   -> regs {s1 = word}
  A0   -> regs {a0 = word}
  A1   -> regs {a1 = word}
  A2   -> regs {a2 = word}
  A3   -> regs {a3 = word}
  A4   -> regs {a4 = word}
  A5   -> regs {a5 = word}
  A6   -> regs {a6 = word}
  A7   -> regs {a7 = word}
  S2   -> regs {s2 = word}
  S3   -> regs {s3 = word}
  S4   -> regs {s4 = word}
  S5   -> regs {s5 = word}
  S6   -> regs {s6 = word}
  S7   -> regs {s7 = word}
  S8   -> regs {s8 = word}
  S9   -> regs {s9 = word}
  S10  -> regs {s10 = word}
  S11  -> regs {s11 = word}
  T3   -> regs {t3 = word}
  T4   -> regs {t4 = word}
  T5   -> regs {t5 = word}
  T6   -> regs {t6 = word}

-- Test function
-- >>> decodeImmBFormat . encodeImmBFormat $ 1 /= 1
-- >>> decodeImmBFormat . encodeImmBFormat $ 2 = 2
-- >>> decodeImmBFormat . encodeImmBFormat $ 3 /= 3
-- >>> decodeImmBFormat . encodeImmBFormat $ 4 = 4
encodeImmBFormat :: Signed 32 -> (Word7, Word5)
encodeImmBFormat imm = (Word7 $ unpack offset7, Word5 $ unpack offset5)
    where
      -- offset1 = imm[12|10:5]
      -- offset2 = imm[4:1|11]
      imm12   = slice Nat.d11 Nat.d11 (pack imm)
      imm10_5 = slice Nat.d10 Nat.d5 (pack imm)
      imm4_1  = slice Nat.d4 Nat.d1 (pack imm)
      imm11   = slice Nat.d0 Nat.d0 (pack imm)
      -- offset = imm12 ++# imm11 ++# imm10_5 ++# imm4_1 ++# (0 :: BitVector 1)
      offset7 = imm12 ++# imm10_5
      offset5 = imm4_1 ++# imm11

decodeImmBFormat :: (Word7, Word5) -> Signed 32
decodeImmBFormat (Word7 offset1, Word5 offset2) = unpack offset
  where
    -- offset1 = imm[12|10:5]
    -- offset2 = imm[4:1|11]
    offset12   = slice Nat.d6 Nat.d6 (pack offset1)
    offset10_5 = slice Nat.d5 Nat.d0 (pack offset1)
    offset4_1  = slice Nat.d4 Nat.d1 (pack offset2)
    offset11   = slice Nat.d0 Nat.d0 (pack offset2)
    offset_bitvector = offset12 ++# offset11 ++# offset10_5 ++# offset4_1 ++# (0 :: BitVector 1) -- BitVector 13
    offset = resize offset_bitvector


-- CPU State
data CPUActivity
  = LoadingInstruction
  | ExecutingInstruction Instruction deriving (Show, Generic, NFDataX)
  -- -- | Halted

data CPUState = CPUState CPUActivity Registers deriving (Show, Generic, NFDataX)


cycle :: HasCallStack => (CPUState, RAM) -> (CPUState, RAM)
cycle (CPUState activity registers, ram) = case activity of
  LoadingInstruction -> (CPUState activity' registers', ram)
  ExecutingInstruction instruction -> case instruction of
    R format -> case format of
      RArith rarith rs2 rs1 rd -> case rarith of
        ADD  -> (CPUState LoadingInstruction registers', ram)
          where
          result     = readRegister registers rs1 + readRegister registers rs2
          registers' = writeRegister registers rd result
        SUB  -> (CPUState LoadingInstruction registers', ram)
          where
            result     = readRegister registers rs1 - readRegister registers rs2
            registers' = writeRegister registers rd result
        AND  -> (CPUState LoadingInstruction registers', ram)
          where
            result     = unpack $ pack (readRegister registers rs1) `and#` pack (readRegister registers rs2)
            registers' = writeRegister registers rd result
        OR   -> (CPUState LoadingInstruction registers', ram)
          where
            result     = unpack $ pack (readRegister registers rs1) `or#` pack (readRegister registers rs2)
            registers' = writeRegister registers rd result
        XOR  -> (CPUState LoadingInstruction registers', ram)
          where
            result     = unpack $ pack (readRegister registers rs1) `xor#` pack (readRegister registers rs2)
            registers' = writeRegister registers rd result
        SLL  -> (CPUState LoadingInstruction registers', ram)
          where
            -- shift value is determined by lower 5 bit of rs2
            -- so, you can get this by 'and' with 0...01_1111
            shift = fromIntegral $ toInteger# $ pack (readRegister registers rs2) `and#` pack (31 :: Signed 32)
            result = unpack $ shiftL# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers rd result
        SRA  -> (CPUState LoadingInstruction registers', ram)
          where
            msb     = msb# $ pack (readRegister registers rs1)
            shift   = fromIntegral $ toInteger# $ pack (readRegister registers rs2) `and#` pack (31 :: Signed 32)
            shifted = shiftR# (pack (readRegister registers rs1)) shift
            result  = unpack $ replaceFromMsr shifted shift msb
            registers' = writeRegister registers rd result
        SRL -> (CPUState LoadingInstruction registers', ram)
          where
            shift = fromIntegral $ toInteger# $ pack (readRegister registers rs2) `and#` pack (31 :: Signed 32)
            result = unpack $ shiftR# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers rd result
        SLT  -> (CPUState LoadingInstruction registers', ram)
          where
            calcmsb = msb# $
              ((pack (readRegister registers rs1)) - (pack (readRegister registers rs2)))
            result = case calcmsb of
              (1 :: Bit) -> 1 -- the 'lt' case
              (0 :: Bit) -> 0 -- the 'geq' case
            registers' = writeRegister registers rd result
        SLTU -> (CPUState LoadingInstruction registers', ram)
          where
            -- Read register value of Signed 32 and convert it to Unsigned
            urs1 = unpack (pack (readRegister registers rs1)) :: Unsigned 32
            urs2 = unpack (pack (readRegister registers rs2)) :: Unsigned 32
            -- Compare as unsigned values
            result = case urs1 < urs2 of
              True -> 1
              False -> 0
            registers' = writeRegister registers rd result
    I format -> case format of
      IArith arith (Word12 im12) rs1 rd -> case arith of
        ADDI  -> (CPUState LoadingInstruction registers', ram)
          where
          result     = readRegister registers rs1 + resize im12
          registers' = writeRegister registers rd result
        SLTI  -> (CPUState LoadingInstruction registers', ram)
            -- because of calculating minus value, we have to consider as 2's-complement
            -- when you use 'pack' method to Signed value,
            -- it will be 2's-complement value
            -- ex.
            --   λ > pack (-2 :: Signed 32)
            --   1111_1111_1111_1111_1111_1111_1111_1110
            --   λ > pack (1 :: Signed 32)
            --   0000_0000_0000_0000_0000_0000_0000_0001
            -- so, we only have to do is,
            --   * compare 'packed' value (that represented as 2's-comp BitVector)
            -- then, we calculate 'subtract' between two bector and check it's msb
            -- s.t, msb is 0 then geq ('>='), else if msb is 1 then lt (<)
            -- ex.
            --   λ > (pack (1 :: Signed 32)) - (pack (-2 :: Signed 32))
            --   0000_0000_0000_0000_0000_0000_0000_0011
            --   λ > (pack (1 :: Signed 32)) - (pack (2 :: Signed 32))
            --   1111_1111_1111_1111_1111_1111_1111_1111
            --   λ > (pack (1 :: Signed 32)) - (pack (1 :: Signed 32))
            --   0000_0000_0000_0000_0000_0000_0000_0000
          where
            calcmsb = msb# ((pack (readRegister registers rs1)) - (pack (resize im12)))
            result = case calcmsb of
              (1 :: Bit) -> 1 -- the 'lt' case
              (0 :: Bit) -> 0 -- the 'geq' case
            registers' = writeRegister registers rd result
        SLTIU -> (CPUState LoadingInstruction registers', ram)
          where
            urs1 = unpack (pack (readRegister registers rs1)) :: Unsigned 32
            urs2 = unpack (pack (resize im12)) :: Unsigned 32
            result = case urs1 < urs2 of
              True -> 1
              False -> 0
            registers' = writeRegister registers rd result
        XORI  -> (CPUState LoadingInstruction registers', ram)
          where
            result     = unpack $ pack (readRegister registers rs1) `xor#` pack (resize im12)
            registers' = writeRegister registers rd result
        ORI   -> (CPUState LoadingInstruction registers', ram)
          where
            result = unpack $ pack (readRegister registers rs1) `or#` pack (resize im12)
            registers' = writeRegister registers rd result
        ANDI  -> (CPUState LoadingInstruction registers', ram)
          where
            result = unpack $ pack (readRegister registers rs1) `and#` pack (resize im12)
            registers' = writeRegister registers rd result
      IShift shift (Word5 shamt) rs1 rd -> case shift of
        SLLI -> (CPUState LoadingInstruction registers', ram)
          where
            shift = case index# (pack shamt) (size# (pack shamt) - 1) of
              0 -> fromIntegral $ toInteger# $ pack shamt
              1 -> 0 -- This case is unexpected in RV32I
            result = unpack $ shiftL# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers rd result
        SRAI -> (CPUState LoadingInstruction registers', ram)
          where
            shift = case index# (pack shamt) (size# (pack shamt) - 1) of
              0 -> fromIntegral $ toInteger# $ pack shamt
              1 -> 0 -- This case is unexpected in RV32I
            msb     = msb# $ pack (readRegister registers rs1)
            shifted = shiftR# (pack (readRegister registers rs1)) shift
            result  = unpack $ replaceFromMsr shifted shift msb
            registers' = writeRegister registers rd result
        SRLI -> (CPUState LoadingInstruction registers', ram)
          where
            shift = case index# (pack shamt) (size# (pack shamt) - 1) of
              0 -> fromIntegral $ toInteger# $ pack shamt
              1 -> 0
            result = unpack $ shiftR# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers rd result
      IJalr jalr (Word12 offset) rs1 rd -> case jalr of
        JALR -> (CPUState LoadingInstruction registers'', ram)
          where
            -- TODO: mismatch of Address space and Vec n Instruction
            Ptr before = pc registers
            registers' = writeRegister registers rd (before + (1 :: Signed 32))
            pc' = Ptr $ unpack $
              replaceBit#
                (pack (readRegister registers rs1 + resize offset))
                0
                0
            registers'' = registers' { pc = pc' }
      ILoad load (Word12 offset) rs1 rd -> case load of
        LB  -> (CPUState LoadingInstruction registers', ram)
          where
            ptr = Ptr $ readRegister registers rs1 + resize offset
            Word8 value = readRAM1Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LH  -> (CPUState LoadingInstruction registers', ram)
          where
            ptr = Ptr $ readRegister registers rs1 + resize offset
            Word16 value = readRAM2Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LW  -> (CPUState LoadingInstruction registers', ram)
          where
            ptr = Ptr $ readRegister registers rs1 + resize offset
            Word32 value = readRAM4Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LBU -> (CPUState LoadingInstruction registers', ram)
          where
            zeroExpand :: Signed 8 -> Signed 32
            zeroExpand num = unpack $ (0 :: BitVector 24) ++# (pack num)
            ptr = Ptr $ readRegister registers rs1 + resize offset
            Word8 value = readRAM1Byte ram ptr
            result = zeroExpand value
            registers' = writeRegister registers rd result
        LHU -> (CPUState LoadingInstruction registers', ram)
            where
              zeroExpand :: Signed 16 -> Signed 32
              zeroExpand num = unpack $ (0 :: BitVector 16) ++# (pack num)
              ptr = Ptr $ readRegister registers rs1 + resize offset
              Word16 value = readRAM2Byte ram ptr
              result = zeroExpand value
              registers' = writeRegister registers rd result
      IFence fence (Word4 pred) (Word4 succ) -> case fence of
        FENCE -> undefined
      IFencei fencei -> case fencei of
        FENCEI -> undefined
      IEnv env -> case env of
        EBREAK -> undefined
        ECALL  -> undefined
      ICsr csr (Word12 c) rs1 rd -> case csr of
        CSRRC -> undefined
        CSRRS -> undefined
        CSRRW -> undefined
      ICsri csri (Word12 c) (Word5 zimm) rd -> case csri of
        CSRRCI -> undefined
        CSRRSI -> undefined
        CSRRWI -> undefined
    S format -> case format of
      SStore store (Word7 offset1) rs2 rs1 (Word5 offset2) -> case store of
        SB -> (CPUState LoadingInstruction registers, ram')
          where
            offset = unpack $ pack offset1 ++# pack offset2
            ptr    = Ptr $ readRegister registers rs1 + resize offset
            value  = Word8 $ resize $ readRegister registers rs2
            ram'   = writeRAM1Byte ram ptr value
        SH -> (CPUState LoadingInstruction registers, ram')
          where
            offset = unpack $ pack offset1 ++# pack offset2
            ptr    = Ptr $ readRegister registers rs1 + resize offset
            value  = Word16 $ resize $ readRegister registers rs2
            ram'   = writeRAM2Byte ram ptr value
        SW -> (CPUState LoadingInstruction registers, ram')
          where
            offset = unpack $ pack offset1 ++# pack offset2
            ptr    = Ptr $ readRegister registers rs1 + resize offset
            value  = Word32 $ readRegister registers rs2
            ram'   = writeRAM4Byte ram ptr value
    B format -> case format of
      BBranch br (Word7 offset1) rs2 rs1 (Word5 offset2) -> case br of
        BEQ  -> (CPUState LoadingInstruction registers', ram)
          where
            offset  = unpack (bformat_offset offset1 offset2)
            pc'     = pc registers
            pc''    = case readRegister registers rs1 == readRegister registers rs2 of
              True  -> addptr pc' (resize offset)
              False -> pc'
            registers' = registers {pc = pc''}
        BNE  -> (CPUState LoadingInstruction registers', ram)
          where
            offset  = unpack (bformat_offset offset1 offset2)
            pc'     = pc registers
            pc''    = case readRegister registers rs1 == readRegister registers rs2 of
              True -> pc'
              False  -> addptr pc' (resize offset)
            registers' = registers {pc = pc''}
        BLT  -> (CPUState LoadingInstruction registers', ram)
          where
            offset = unpack (bformat_offset offset1 offset2)
            pc'    = pc registers
            calcmsb = msb# ((pack (readRegister registers rs1)) - (pack (readRegister registers rs2)))
            pc'' = case calcmsb of
              (1 :: Bit) -> addptr pc' (resize offset)
              (0 :: Bit) -> pc'
            registers' = registers {pc = pc''}
        BGE  -> (CPUState LoadingInstruction registers', ram)
          where
            offset = unpack (bformat_offset offset1 offset2)
            pc'    = pc registers
            calcmsb = msb# ((pack (readRegister registers rs1)) - (pack (readRegister registers rs2)))
            pc'' = case calcmsb of
              (1 :: Bit) -> pc'
              (0 :: Bit) -> addptr pc' (resize offset)
            registers' = registers {pc = pc''}
        BLTU -> (CPUState LoadingInstruction registers', ram)
          where
            -- Read register value of Signed 32 and convert it to Unsigned
            urs1 = unpack (pack (readRegister registers rs1)) :: Unsigned 32
            urs2 = unpack (pack (readRegister registers rs2)) :: Unsigned 32
            offset = unpack (bformat_offset offset1 offset2)
            pc'    = pc registers
            pc'' = case urs1 < urs2 of
              True -> addptr pc' (resize offset)
              False -> pc'
            registers' = registers {pc = pc''}
        BGEU -> (CPUState LoadingInstruction registers', ram)
          where
            -- Read register value of Signed 32 and convert it to Unsigned
            urs1 = unpack (pack (readRegister registers rs1)) :: Unsigned 32
            urs2 = unpack (pack (readRegister registers rs2)) :: Unsigned 32
            offset = unpack (bformat_offset offset1 offset2)
            -- offset = unpack $ pack offset1 ++# pack offset2
            pc'    = pc registers
            pc'' = case urs1 < urs2 of
              True -> pc'
              False -> addptr pc' (resize offset)
            registers' = registers {pc = pc''}
      where
        bformat_offset :: Signed 7 -> Signed 5 -> BitVector 13
        bformat_offset offset1 offset2 = offset_bitvector
          where
            offset12   = slice Nat.d6 Nat.d6 (pack offset1)
            offset10_5 = slice Nat.d5 Nat.d0 (pack offset1)
            offset4_1  = slice Nat.d4 Nat.d1 (pack offset2)
            offset11   = slice Nat.d0 Nat.d0 (pack offset2)
            offset_bitvector =
              offset12 ++#
              offset11 ++#
              offset10_5 ++#
              offset4_1 ++#
              (0 :: BitVector 1)
    U format -> case format of
      UArith arith (Word20 im20) rd -> case arith of
        LUI   -> (CPUState LoadingInstruction registers', ram)
          where
            result = unpack $ shiftL# (resize $ pack im20) 12
            registers' = writeRegister registers rd result
        AUIPC -> (CPUState LoadingInstruction registers', ram)
          where
            shifted = unpack $ shiftL# (resize $ pack im20) 12
            Ptr ad  = pc registers
            result  = shifted + ad
            registers' = writeRegister registers rd result
    J format -> case format of
      JJal jal (Word20 offset) rd -> case jal of
        JAL -> (CPUState LoadingInstruction registers'', ram)
          where
            -- TODO: mismatch of Address space and Vec n Instruction
            Ptr addr = pc registers
            -- registers' = writeRegister registers rd (addr + (4 :: Signed 32))
            registers' = writeRegister registers rd (addr + (1 :: Signed 32))
            pc' = Ptr (resize offset)
            registers'' = registers' { pc = pc' }
  where
    loadedWord = readRAM ram (pc registers)
    activity'  = ExecutingInstruction (decodeInstruction loadedWord)
    registers' = registers {pc = increment (pc registers)}

replaceFromMsr :: BitVector 32 -> Int -> Bit -> BitVector 32
replaceFromMsr bv shift bit = loop bv size shift bit
  where
    size = (size# bv) - 1
    loop :: BitVector 32 -> Int -> Int -> Bit -> BitVector 32
    loop bv index shift bit
      | index == (size - shift) = bv
      | otherwise               = loop (replaceBit# bv index bit) (index - 1) shift bit


cpuHardware :: (HiddenClockResetEnable dom) => CPUState -> RAM -> Signal dom Registers
cpuHardware initialCPUState initialRAM = resultSignal
  where
    systemState  :: (HiddenClockResetEnable dom) => Signal dom (CPUState, RAM)
    systemState  = register (initialCPUState, initialRAM) systemState'
    systemState' :: (HiddenClockResetEnable dom) => Signal dom (CPUState, RAM)
    systemState' = fmap cycle systemState
    execute :: (CPUState, RAM) -> Registers
    execute ((CPUState _ registers), _) = registers
    resultSignal :: (HiddenClockResetEnable dom) => Signal dom Registers
    resultSignal = fmap execute systemState'

initCPUState :: CPUState
initCPUState = zeroRegisterCPU (Ptr 0)

zeroRegisterCPU :: Ptr -> CPUState
zeroRegisterCPU ptr = CPUState LoadingInstruction (Registers 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ptr)

testAddProgramWithMinusImm = I (IArith ADDI (Word12 (-1 :: Signed 12)) S2 S3) :>
                             I (IArith ADDI (Word12 (2 :: Signed 12)) S2 S4)  :>
                             R (RArith ADD S3 S4 S2) :>
                             J (JJal JAL (Word20 3) S5) :> -- loop
                             Nil


-- RISC-V向けにンパイルしたコードのBinaryを直書きして実行してみる
-- 標準ライブラリとか使うと、それらをメモリ上に展開する必要があるので、一旦そこまではなし
-- 単純なfibonacci計算のコードにする。ELFLoaderも考えたくないのでbuildしたものをobjdumpして
-- assembly code飲み取り出して、Instructionに変換する
-- > 詳細 - src/RV32I/Programs/Fib.hs

-- TODO: Unsigned -> Signedじゃないとだめ
-- TODO: offsetは基本0x
-- TODO: Addressはどうすればいいんだろう。。。先頭から数えるでいいのかな。。
-- -- human convert : disassem -> instruction
-- fibFunc = I (IArith ADDI (Word20 -32) SP SP) :> -- addi  sp,sp,-32
--           S (SStore SW (Word7 X) RA SP (Word5 Y) ) :> -- sw  ra,28(sp)
--           -- Word7+Word5でoffset=28を表現する必要がある
--           -- Signedで拡張したあとに計算できれば楽(別々にやると負数で困る)
--           -- offset=28 = BitVector=0000_0001_1100 => Word7 = 0000000, Word5=11100 (Unsiged)
--           -- Signedになると変わるのでこれではまずい?(これはoffset=word12で取れば別.結局負数が取れないのでだめだけど)
--           -- offset1/offset2のようになっているformatでは、片方がUnsigned/片方がSignedになるかんじ。（両方足してUnsigned）
--           -- とりあえず引き続き書いていってみるか。。。修正かなり大掛かりになりそう
--           S (SStore SW (Word7 X) S0 SP (Word5 Y)) :> -- sw  s0,24(sp) X+Y=24
--           S (SStore SW (Word7 X) S1 SP (Word5 Y)) :> -- sw  s1,20(sp) X+Y=20
--           I (IArith ADDI (Word20 32) S0 SP)       :> -- addi  s0,sp,32
--           S (SStore SW (Word7 X) A0 S0 (Word5 Y)) :> -- sw  a0,-20(s0) X+Y=-20
--           I (ILoad  LW (Word12 -20) S0 A4         :> -- lw  a4,-20(s0)
--           U (UArith LUI (Word20 1) A5             :> -- li  a5,1 (-> lui/addiに展開される。ここではluiを選択)
--           B (BBranch BLT (Word7 X) A4 A5 (Word5 Y) :> -- blt  a5,a4,10170 X+Y=10170
--           U (UArith LUI (Word23 1) A5             :> -- li  a5,1
--           J (JJal JAL (Word20 1019c) Zero)        :> -- j  1019c (-> jal x0,1019cに展開される)
--           I (ILoad LW (Word12 -20) S0 A5          :> -- lw  a5,-20(s0)
--           I (IArith ADDI (Word20 -1) A5 A5)       :> -- addi  a5,a5,-1
--           I (IArith ADDI (Word20 0) A5 A0)        :> -- mv  a0,a5 (-> addi a0,a5,0)
--           J (JJal JAL (Word20 10144) RA)          :> -- jal  ra,10144
--           I (IArith ADDI (Word20 0) A0,S1)        :> -- mv  s1,a0
--           I (ILoad LW (Word12 -20) S0 A5          :> -- lw  a5,-20(s0)
--           I (IArith ADDI (Word20 -2) A5 A5)       :> -- addi  a5,a5,-2
--           I (IArith ADDI (Word20 0) A5 A0)        :> -- mv  a0,a5 (-> addi a0,a5,0)
--           J (JJal JAL (Word20 10144) RA)          :> -- jal  ra,10144
--           I (IArith ADDI (Word20 0) A0 A5)        :> -- mv  a5,a0 (-> addi a5,a0,0)
--           R (RArith ADD A5 S1 A5)                 :> -- add  a5,s1,a5
--           I (IArith ADDI (Word20 0) A5 A0)        :> -- mv  a0,a5 (-> addi a0,a5,0)
--           I (ILoad LW (Word12 28) SP RA)          :> -- lw  ra,28(sp)
--           I (ILoad LW (Word12 24) SP S0)          :> -- lw  s0,24(sp)
--           I (ILoad LW (Word12 20) SP S1)          :> -- lw  s1,20(sp)
--           I (IArith ADDI (Word20 32) SP SP)       :> -- addi  sp,sp,32
--           I (IJalr JALR (Word12 0) RA Zero        :> -- ret (-> jalr x0 0(x1)に展開される)
--           Nil
--
-- mainFunc = I (IArith ADDI (Word20 -32) SP SP)     :> -- addi  sp,sp,-32
--            S (SStore SW (Word7 X) RA SP (Word5 Y) :> -- sw  ra,28(sp) X+Y=28
--            S (SStore SW (Word7 X) S0 SP (Word5 Y) :> -- sw  s0,24(sp) X+Y=24
--            I (IArith ADDI (Word20 32) SP S0)      :> -- addi  s0,sp,32
--            U (UArith LUI (Word20 10) A0           :> -- li  a0,10 (-> lui a0,10)
--            J (JJal JAL (Word20 10144) RA)         :> -- jal  ra,10144
--            S (SStore SW (Word7 X) A0 S0 (Word5 Y) :> -- sw  a0,-20(s0) X+Y=-20
--            J (JJal JAL (Word20 101d0) Zero)       :> j  101d0 (-> jal x0,101d0 に展開される)
--
--
-- 関数別にメモリロードできないのでくっつける
-- Jalの先はメモリ上の値を0~で見ていったときの値


-- Address calculationなど含めスプレッドシートに記載したので必要によっては確認されたし
fibonacciProgram = fibProgram
  where
    s_slice7 :: Signed 12 -> Signed 7
    s_slice7 num = unpack (slice Nat.d11 Nat.d5 (pack num))
    s_slice5 :: Signed 12 -> Signed 5
    s_slice5 num = unpack (slice Nat.d4 Nat.d0 (pack num))
    b_slice12 :: Signed 13 -> Signed 1
    b_slice12 num = unpack (slice Nat.d12 Nat.d12 (pack num))
    b_slice11 :: Signed 13 -> Signed 1
    b_slice11 num = unpack (slice Nat.d11 Nat.d11 (pack num))
    b_slice10_5 :: Signed 13 -> Signed 6
    b_slice10_5 num = unpack (slice Nat.d10 Nat.d5 (pack num))
    b_slice4_1 :: Signed 13 -> Signed 4
    b_slice4_1 num = unpack (slice Nat.d4 Nat.d1 (pack num))
    b_offset_12_10_5 :: Signed 13 -> Signed 7
    b_offset_12_10_5 num = unpack $ (pack (b_slice12 num)) ++# (pack (b_slice10_5 num))
    b_offset_4_1_11 :: Signed 13 -> Signed 5
    b_offset_4_1_11 num = unpack $ (pack (b_slice4_1 num)) ++# (pack (b_slice11 num))
    -- Helper functions to convert binary instruction to RISC-V instruction format
    addi rd  rs1    imm = I (IArith ADDI (Word12 imm) rs1 rd)
    add  rd  rs1    rs2 = R (RArith ADD rs2 rs1 rd)
    sw   rs2 offset rs1 = S (SStore SW (Word7 (s_slice7 (offset :: Signed 12))) rs2 rs1 (Word5 (s_slice5 (offset :: Signed 12))))
    lw   rd  offset rs1 = I (ILoad LW (Word12 (offset :: Signed 12)) rs1 rd)
    li   rd  imm        = addi rd Zero imm
    blt  rs1 rs2 offset = B (BBranch BLT (Word7 (b_offset_12_10_5 offset)) rs2 rs1 (Word5 (b_offset_4_1_11 offset)))
    mv   rd  rs1        = addi rd rs1 0
    jal  rd  offset     = J (JJal JAL (Word20 offset) rd)
    jalr rd  offset rs1 = I (IJalr JALR (Word12 offset) rs1 rd)
    j    offset         = jal Zero offset
    ret                 = jalr Zero 0 RA
    -- Fib Program
    -- > ちょっとおかしいかもしれない。確認jmp周り
    fibProgram =
      -- start Fibonacci function from here
         addi SP SP (-10) -- from here
      :> sw   RA 1  SP
      :> sw   S0 2  SP
      :> sw   S1 3  SP
      :> addi S0 SP 10
      :> sw   A0 (-1) S0
      :> lw   A4 (-1) S0
      :> li   A5 1
      :> blt  A5 A4 3
      :> lw   A5 (-1) S0
      :> addi A5 A5 (-1)
      :> mv   A0 A5
      :> jal  RA 0
      :> li   A5 1
      :> j    26
      :> lw   A5 (-20) S0
      :> addi A5 A5 (-1)
      :> mv   A0 A5
      :> jal  RA 0
      :> mv   S1 A0
      :> lw   A5 (-20) S0
      :> addi A5 A5 (-2)
      :> mv   A0 A5
      :> jal  RA 5
      :> mv   A5 A0
      :> add  A5 S1 A5
      :> mv   A0 A5
      :> lw   RA 28 SP
      :> lw   S0 24 SP
      :> lw   S1 20 SP
      :> addi SP SP 32
      :> ret
      -- start main function
      :> addi SP SP 1000 -- Ptr = 32
      :> addi SP SP (-10)
      :> sw   RA 1  SP -- Need to Debug (MEMORY ADDRESS)
      :> sw   S0 2  SP
      :> addi S0 SP 10
      :> li   A0 10
      :> jal  RA 0 -- jamp to Fibonacci
      -- :> sw
      :> j 39  -- jamp here
      :> Nil

    -- myProgram =
    --      I (IArith ADDI (Word12 10)  Zero A0)
    --   -- TODO: required to change s0 to sp
    --   :> I (IArith ADDI (Word12 100) Zero S0) -- 32 -> 100 because of the memory (conflict the instruction and saved data)
    --   -- TODO: S0 -> SP is required?
    --   :> S (SStore SW (Word7 (s_slice7 (0   :: Signed 12))) RA   S0 (Word5 (s_slice5 (0   :: Signed 12))))
    --   :> S (SStore SW (Word7 (s_slice7 (-4  :: Signed 12))) S0 S0 (Word5 (s_slice5 (-4  :: Signed 12))))
    --   :> S (SStore SW (Word7 (s_slice7 (-8  :: Signed 12))) S1   S0 (Word5 (s_slice5 (-8  :: Signed 12))))
    --   :> S (SStore SW (Word7 (s_slice7 (-12 :: Signed 12))) A0   S0 (Word5 (s_slice5 (-12 :: Signed 12))))
    --   :> I (ILoad  LW (Word12 (-12 :: Signed 12)) S0 A4)
    --   :> I (IArith ADDI (Word12 1) Zero A5)
    --   :> B (BBranch BLT (Word7 (b_offset_12_10_5 3)) A4 A5 (Word5 (b_offset_4_1_11 3)))
    --   :> I (IArith ADDI (Word12 1) Zero A5)
    --   :> J (JJal JAL (Word20 22) S10)
    --   -- fib(n-1) branch
    --   :> I (ILoad LW (Word12 (-12 :: Signed 12)) S0 A5) -- start to calculate fib(n-1)
    --   :> I (IArith ADDI (Word12 (-1 :: Signed 12)) A5 A5)
    --   :> I (IArith ADDI (Word12 0) A5 A0)
    --   :> J (JJal JAL (Word20 1) RA) -- loop fibonacci RA=15, pc=1
    --   :> I (IArith ADDI (Word12 0) A0 S1) -- store the result of fib(n-1) to s1
    --   -- fib(n-2) branch
    --   :> I (ILoad LW (Word12 (-20 :: Signed 12)) S0 A5) -- start to calculate fib(n-2)
    --   :> I (IArith ADDI (Word12 (-2 :: Signed 12)) A5 A5)
    --   :> I (IArith ADDI (Word12 0) A5 A0)
    --   :> J (JJal JAL (Word20 1) RA) -- loop fibonacci
    --   :> I (IArith ADDI (Word12 0) A0 A5) -- load the value of A0 to A5
    --   :> R (RArith ADD A5 S1 A5) -- calculate A5(=fib(n-2)) + S1(=fib(n-1)) and store it to A5
    --   :> I (IArith ADDI (Word12 0) A5 A0) -- copy the result of A5 to A0
    --   -- TODO: required to change sp
    --   :> I (ILoad LW (Word12 (0 :: Signed 12)) S0 RA)
    --   :> I (ILoad LW (Word12 (-4 :: Signed 12)) S0 S0)
    --   :> I (ILoad LW (Word12 (-8 :: Signed 12)) S0 S1)
    --   :> I (IArith ADDI (Word12 100) S0 S0)
    --   :> I (IJalr JALR (Word12 0) RA Zero) -- Ret represents JALR
    --   :> J (JJal JAL (Word20 28) S11)
    --   :> Nil
    -- -- TODO: reference sample program
    -- program =
    --   -- TODO: UArith is wrong? -> write as Addi above
    --   -- -- 関数呼び出しに伴うStack Pointer周りの動きになるので一旦ここでは省略
    --   --    I (IArith ADDI (Word12 (-32 :: Signed 12)) SP SP)                                                 -- addi  sp,sp,-32
    --   -- :> S (SStore SW (Word7 (s_slice7 28)) RA SP (Word5 (s_slice5 28)))                                   -- sw    ra,28(sp) X+Y=28 28 :: Signed 12 -> partition
    --   -- :> S (SStore SW (Word7 (s_slice7 24)) S0 SP (Word5 (s_slice5 24)))                                 -- sw    s0,24(sp) X+Y=24
    --   -- :> I (IArith ADDI (Word12 32) SP S0)                                                               -- addi  s0,sp,32
    --      U (UArith LUI (Word20 10) A0)                                                                     -- li    a0,10 (-> lui a0,10)
    --   -- :> J (JJal JAL (Word20 10144) RA)                                                                 -- jal   ra,10144 (=> fib call -> hardcode)
    --   -- fib function hardcode start >>>>>>>>>>>>
    --   -- TODO: いくつか処理を落とす必要があるかも
    --   -- :> I (IArith ADDI (Word12 (-32 :: Signed 12)) SP SP)                                                 -- addi  sp,sp,-32
    --   :> S (SStore SW (Word7 (s_slice7 0)) RA SP (Word5 (s_slice5 28)))                                   -- sw    ra,28(sp)
    --   :> S (SStore SW (Word7 (s_slice7 4)) S0 SP (Word5 (s_slice5 24)))                                 -- sw    s0,24(sp) X+Y=24
    --   :> S (SStore SW (Word7 (s_slice7 20)) S1 SP (Word5 (s_slice5 20)))                                   -- sw    s1,20(sp) X+Y=20
    --   :> I (IArith ADDI (Word12 32) S0 SP)                                                               -- addi  s0,sp,32
    --   :> S (SStore SW (Word7 (s_slice7 (-20 :: Signed 12))) A0 S0 (Word5 (s_slice5 (-20 :: Signed 12)))) -- sw    a0,-20(s0) X+Y=-20
    --   :> I (ILoad  LW (Word12 (-20 :: Signed 12)) S0 A4)                                                 -- lw    a4,-20(s0)
    --   :> U (UArith LUI (Word20 1) A5)                                                                      -- li    a5,1 (-> lui/addiに展開される。ここではluiを選択)
    --   :> B (BBranch BLT (Word7 (b_offset_12_10_5 16)) A4 A5 (Word5 (b_offset_4_1_11 16)))                  -- blt   a5,a4,10170 X+Y=10170 (10170はfibの計算途中のアドレス) => a5,a4,16
    --   :> U (UArith LUI (Word20 1) A5)                                                                      -- li    a5,1
    --   :> J (JJal JAL (Word20 27) Zero)                                                                     -- j     1019c (-> jal x0,1019cに展開される / 1019cはfibの再帰終了の流れのaddress) => j 27
    --   :> I (ILoad LW (Word12 (-20 :: Signed 12)) S0 A5)                                                  -- lw    a5,-20(s0)
    --   :> I (IArith ADDI (Word12 (-1 :: Signed 12)) A5 A5)                                                  -- addi  a5,a5,-1
    --   :> I (IArith ADDI (Word12 0) A5 A0)                                                                  -- mv    a0,a5 (-> addi a0,a5,0)
    --   :> J (JJal JAL (Word20 5) RA)                                                                        -- jal   ra,10144 (=> 10144 = fibonacci関数の先頭) => ra,5
    --   :> I (IArith ADDI (Word12 0) A0 S1)                                                                  -- mv    s1,a0
    --   :> I (ILoad LW (Word12 (-20 :: Signed 12)) S0 A5)                                                  -- lw    a5,-20(s0)
    --   :> I (IArith ADDI (Word12 (-2 :: Signed 12)) A5 A5)                                                  -- addi  a5,a5,-2
    --   :> I (IArith ADDI (Word12 0) A5 A0)                                                                  -- mv    a0,a5 (-> addi a0,a5,0)
    --   :> J (JJal JAL (Word20 5) RA)                                                                        -- jal   ra,10144 => jal ra,5
    --   :> I (IArith ADDI (Word12 0) A0 A5)                                                                  -- mv    a5,a0 (-> addi a5,a0,0)
    --   :> R (RArith ADD A5 S1 A5)                                                                           -- add   a5,s1,a5
    --   :> I (IArith ADDI (Word12 0) A5 A0)                                                                  -- mv    a0,a5 (-> addi a0,a5,0)
    --   :> I (ILoad LW (Word12 28) SP RA)                                                                    -- lw    ra,28(sp)
    --   :> I (ILoad LW (Word12 24) SP S0)                                                                  -- lw    s0,24(sp)
    --   :> I (ILoad LW (Word12 20) SP S1)                                                                    -- lw    s1,20(sp)
    --   :> I (IArith ADDI (Word12 32) SP SP)                                                                 -- addi  sp,sp,32
    --   -- :> I (IJalr JALR (Word12 0) RA Zero                                                               -- ret   (-> jalr x0 0(x1)に展開される) -- 今回returnはない
    --   -- :> Nil
    --   -- <<<<<<<<<<<<< fib function hardcode stop
    --   :> S (SStore SW (Word7 (s_slice7 (-20 :: Signed 12))) A0 S0 (Word5 (s_slice5 (-20 :: Signed 12)))) -- sw    a0,-20(s0) X+Y=-20
    --   :> J (JJal JAL (Word20 33) Zero)                                                                     -- j     101d0 (-> jal x0,101d0 に展開される/自身へのループなのでアドレス変更) => 33
    --   :> Nil

programMem = fmap encodeInstruction fibonacciProgram

programmedRAM :: (KnownNat n, KnownNat m, (n + m) ~ 1000) => Vec n Instruction -> RAM
programmedRAM inst = RAM $ paddingEncodedInstruction (fmap encodeInstruction inst) (repeat (Word32 0))

paddingEncodedInstruction ::
  forall n m . (KnownNat n, KnownNat m, (n + m) ~ 1000)
    => Vec n Word32
    -> Vec m Word32
    -> Vec 1000 Word32
paddingEncodedInstruction inst zeroInst = inst ++ zeroInst


programCpu :: HiddenClockResetEnable dom => Signal dom Registers
programCpu = cpuHardware (zeroRegisterCPU (Ptr 32)) (RAM (programMem ++ repeat (Word32 0)))

-- You can play code in RV32I.RV32I.Programs.Example like follow
-- >>> Prelude.take 10 $ sample (cpu initCPUState (programmedRAM addImmValues) :: Signal System Registers)
cpu :: HiddenClockResetEnable dom => CPUState -> RAM -> Signal dom Registers
cpu = cpuHardware

programResult :: [Registers]
programResult = take 100 $ sample (programCpu :: Signal System Registers)

programOutput :: IO ()
programOutput = mapM_ print programResult

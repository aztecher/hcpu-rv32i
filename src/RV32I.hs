module RV32I where

import Clash.Prelude (slice)
import Clash.Promoted.Nat.Literals as Nat
import Clash.Sized.BitVector (BitVector, Bit, (++#), size#)
import Clash.Sized.Internal.BitVector
  (and#, or#, xor#, toInteger#, shiftL#, shiftR#, index#,
   msb#, replaceBit#, lt#, gt#, ge#, complement#, neq#)
import Clash.Sized.Unsigned (Unsigned)
import Clash.Class.BitPack (pack, unpack)
import Clash.Class.Resize (resize)
import Clash.Sized.Vector (Vec((:>), Nil), (!!), replace, repeat, (++))
import Clash.Signal (Signal, System, HiddenClockResetEnable, register, sample, exposeClockResetEnable)
import Clash.Signal.Internal (Domain, Clock, Reset, Enable)
import Clash.Explicit.Signal (systemClockGen, systemResetGen)
import Clash.Signal.Internal (enableGen)
import Clash.XException (NFDataX)
import GHC.Generics (Generic)

import Prelude ((+), (-), ($), (==), (>=), (<),
  otherwise, undefined, fromIntegral, fmap, id, take, print,
  Show, IO, Int, Bool(..), Maybe(..))
import Data.Function (fix)

-- Word
newtype Word32 = Word32 (Unsigned 32) deriving (Show, Generic, NFDataX)
newtype Word20 = Word20 (Unsigned 20) deriving (Show, Generic, NFDataX)
newtype Word16 = Word16 (Unsigned 16) deriving (Show, Generic, NFDataX)
newtype Word12 = Word12 (Unsigned 12) deriving (Show, Generic, NFDataX)
newtype Word8  = Word8  (Unsigned  8) deriving (Show, Generic, NFDataX)
newtype Word7  = Word7  (Unsigned  7) deriving (Show, Generic, NFDataX)
newtype Word5  = Word5  (Unsigned  5) deriving (Show, Generic, NFDataX)
newtype Word4  = Word4  (Unsigned  4) deriving (Show, Generic, NFDataX)

-- RAM

data RAM = RAM (Vec 32 Word32) deriving (Show, Generic, NFDataX)

-- Program ... Vector Instruction
--  -(encode)-> Vector Word32
--  -(load)-> RAM (Vector Word32)
--

-- Register

data Register
  = X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X17
  | X18
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28
  | X29
  | X30
  | X31
  deriving (Show, Generic, NFDataX)

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
  deriving (Show, Generic, NFDataX)

-- I Opcode

data IArith
  = ADDI
  | SLTI
  | SLTIU
  | XORI
  | ORI
  | ANDI
  deriving (Show, Generic, NFDataX)

data IShift
  = SLLI
  | SRAI
  | SRLI
  deriving (Show, Generic, NFDataX)

data IJalr = JALR deriving (Show, Generic, NFDataX)

data ILoad
  = LB
  | LH
  | LW
  | LBU
  | LHU
  deriving (Show, Generic, NFDataX)

data IFence = FENCE deriving (Show, Generic, NFDataX)

data IFencei = FENCEI deriving (Show, Generic, NFDataX)

data IEnv
  = EBREAK
  | ECALL
  deriving (Show, Generic, NFDataX)

data ICsr
  = CSRRC
  | CSRRS
  | CSRRW
  deriving (Show, Generic, NFDataX)

data ICsri
  = CSRRCI
  | CSRRSI
  | CSRRWI
  deriving (Show, Generic, NFDataX)

-- S Opcode

data SStore
  = SB
  | SH
  | SW
  deriving (Show, Generic, NFDataX)

-- B Opcode
data BBranch
  = BEQ
  | BNE
  | BLT
  | BGE
  | BLTU
  | BGEU
  deriving (Show, Generic, NFDataX)

-- U Opcode
data UArith
  = LUI
  | AUIPC
  deriving (Show, Generic, NFDataX)

-- J Opcode
data JJal = JAL deriving (Show, Generic, NFDataX)


-- Format
-- TODO(mmichish) : you can modify definition for implementation

-- R Format
data RFormat
  = RArith RArith
           Register
           Register
           Register
  deriving (Show, Generic, NFDataX)

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
  deriving (Show, Generic, NFDataX)

-- S Format
data SFormat
  = SStore SStore
           Word7
           Register
           Register
           Word5
  deriving (Show, Generic, NFDataX)

-- B Format
data BFormat
  = BBranch BBranch
            Word7
            Register
            Register
            Word5
  deriving (Show, Generic, NFDataX)

-- U Format
data UFormat
  = UArith UArith
           Word20
           Register
  deriving (Show, Generic, NFDataX)

-- J Format
data JFormat
  = JJal JJal
         Word20
         Register
  deriving (Show, Generic, NFDataX)

-- RV32I instruction
data Instruction
  = R RFormat
  | I IFormat
  | S SFormat
  | B BFormat
  | U UFormat
  | J JFormat
  deriving (Show, Generic, NFDataX)


-- CPU Registers
data Registers = Registers {
  x0 :: Unsigned 32,
  x1 :: Unsigned 32,
  x2 :: Unsigned 32,
  x3 :: Unsigned 32,
  x4 :: Unsigned 32,
  x5 :: Unsigned 32,
  x6 :: Unsigned 32,
  x7 :: Unsigned 32,
  x8 :: Unsigned 32,
  x9 :: Unsigned 32,
  x10 :: Unsigned 32,
  x11 :: Unsigned 32,
  x12 :: Unsigned 32,
  x13 :: Unsigned 32,
  x14 :: Unsigned 32,
  x15 :: Unsigned 32,
  x16 :: Unsigned 32,
  x17 :: Unsigned 32,
  x18 :: Unsigned 32,
  x19 :: Unsigned 32,
  x20 :: Unsigned 32,
  x21 :: Unsigned 32,
  x22 :: Unsigned 32,
  x23 :: Unsigned 32,
  x24 :: Unsigned 32,
  x25 :: Unsigned 32,
  x26 :: Unsigned 32,
  x27 :: Unsigned 32,
  x28 :: Unsigned 32,
  x29 :: Unsigned 32,
  x30 :: Unsigned 32,
  x31 :: Unsigned 32,
  pc  :: Ptr
} deriving (Show, Generic, NFDataX)


-- RV32I encoder / decoder

encodeRegister :: Register -> BitVector 5
encodeRegister X0 = 0
encodeRegister X1 = 1
encodeRegister X2 = 2
encodeRegister X3 = 3
encodeRegister X4 = 4
encodeRegister X5 = 5
encodeRegister X6 = 6
encodeRegister X7 = 7
encodeRegister X8 = 8
encodeRegister X9 = 9
encodeRegister X10 = 10
encodeRegister X11 = 11
encodeRegister X12 = 12
encodeRegister X13 = 13
encodeRegister X14 = 14
encodeRegister X15 = 15
encodeRegister X16 = 16
encodeRegister X17 = 17
encodeRegister X18 = 18
encodeRegister X19 = 19
encodeRegister X20 = 20
encodeRegister X21 = 21
encodeRegister X22 = 22
encodeRegister X23 = 23
encodeRegister X24 = 24
encodeRegister X25 = 25
encodeRegister X26 = 26
encodeRegister X27 = 27
encodeRegister X28 = 28
encodeRegister X29 = 29
encodeRegister X30 = 30
encodeRegister X31 = 31

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
decodeRegister 0 = X0
decodeRegister 1 = X1
decodeRegister 2 = X2
decodeRegister 3 = X3
decodeRegister 4 = X4
decodeRegister 5 = X5
decodeRegister 6 = X6
decodeRegister 7 = X7
decodeRegister 8 = X8
decodeRegister 9 = X9
decodeRegister 10 = X10
decodeRegister 11 = X11
decodeRegister 12 = X12
decodeRegister 13 = X13
decodeRegister 14 = X14
decodeRegister 15 = X15
decodeRegister 16 = X16
decodeRegister 17 = X17
decodeRegister 18 = X18
decodeRegister 19 = X19
decodeRegister 20 = X20
decodeRegister 21 = X21
decodeRegister 22 = X22
decodeRegister 23 = X23
decodeRegister 24 = X24
decodeRegister 25 = X25
decodeRegister 26 = X26
decodeRegister 27 = X27
decodeRegister 28 = X28
decodeRegister 29 = X29
decodeRegister 30 = X30
decodeRegister 31 = X31


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

-- 手でTest書くのしんどいのでQuickCheckあたり使いたいな。。。
-- λ > decodeInstruction $ encodeInstruction $  S (SStore SB (Word7 1) X0 X1 (Word5 1))
-- S (SStore SB (Word7 1) X0 X1 (Word5 1))


-- RAM / CPU

-- -- TODO(mmichish): want this
-- 1. write program
-- program :: Vec N Instruction
-- program = Instruction XX XX :>
--           Instruction XX XX :>
--           ......

-- 2. load program in CPU and prepair to execute
-- programCPU :: cpuHardware defaultCPUStatus (RAM program)
--
-- cpuHardware :: CPUState -> RAM -> Signal dom Result
-- defaultCPUStatus :: CPUState
--
-- cpuHardware calls cycle function
--
-- cycle :: (CPUState, RAM) -> (CPUState, RAM)
-- CPUState ... XXXInstruction
-- RAM ... Data


newtype Ptr = Ptr (Unsigned 32) deriving (Show, Generic, NFDataX)

newtype Output = Output (Unsigned 32) deriving (Show, Generic, NFDataX)

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

addptr :: Ptr -> Unsigned 32 -> Ptr
addptr (Ptr address) offset = Ptr (address + offset)

readRegister :: Registers -> Register -> Unsigned 32
readRegister (Registers x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 _) reg = case reg of
  X0 -> x0
  X1 -> x1
  X2 -> x2
  X3 -> x3
  X4 -> x4
  X5 -> x5
  X6 -> x6
  X7 -> x7
  X8 -> x8
  X9 -> x9
  X10 -> x10
  X11 -> x11
  X12 -> x12
  X13 -> x13
  X14 -> x14
  X15 -> x15
  X16 -> x16
  X17 -> x17
  X18 -> x18
  X19 -> x19
  X20 -> x20
  X21 -> x21
  X22 -> x22
  X23 -> x23
  X24 -> x24
  X25 -> x25
  X26 -> x26
  X27 -> x27
  X28 -> x28
  X29 -> x29
  X30 -> x30
  X31 -> x31

writeRegister :: Registers -> Register -> Unsigned 32 -> Registers
writeRegister regs reg word = case reg of
  X0 -> regs {x0 = word}
  X1 -> regs {x1 = word}
  X2 -> regs {x2 = word}
  X3 -> regs {x3 = word}
  X4 -> regs {x4 = word}
  X5 -> regs {x5 = word}
  X6 -> regs {x6 = word}
  X7 -> regs {x7 = word}
  X8 -> regs {x8 = word}
  X9 -> regs {x9 = word}
  X10 -> regs {x10 = word}
  X11 -> regs {x11 = word}
  X12 -> regs {x12 = word}
  X13 -> regs {x13 = word}
  X14 -> regs {x14 = word}
  X15 -> regs {x15 = word}
  X16 -> regs {x16 = word}
  X17 -> regs {x17 = word}
  X18 -> regs {x18 = word}
  X19 -> regs {x19 = word}
  X20 -> regs {x20 = word}
  X21 -> regs {x21 = word}
  X22 -> regs {x22 = word}
  X23 -> regs {x23 = word}
  X24 -> regs {x24 = word}
  X25 -> regs {x25 = word}
  X26 -> regs {x26 = word}
  X27 -> regs {x27 = word}
  X28 -> regs {x28 = word}
  X29 -> regs {x29 = word}
  X30 -> regs {x30 = word}
  X31 -> regs {x31 = word}

-- -- TODO(mmichish): CPU State and RAM
data CPUActivity
  = LoadingInstruction
  | ExecutingInstruction Instruction
  -- | Outputting Output
  -- | Halted
  deriving (Show, Generic, NFDataX)


data CPUState = CPUState CPUActivity Registers deriving (Show, Generic, NFDataX)


-- TODO: all values are Unsigned type...
--       use Signed / Unsigned in case
--       some instruction are not implemented...
cycle :: (CPUState, RAM) -> (CPUState, RAM)
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
            shift = fromIntegral $ toInteger# $ pack (readRegister registers rs2) `and#` pack (31 :: Unsigned 32)
            result = unpack $ shiftL# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers rd result
        SRA  -> (CPUState LoadingInstruction registers', ram)
          where
            msb     = msb# $ pack (readRegister registers rs1)
            shift   = fromIntegral $ toInteger# $ pack (readRegister registers rs2) `and#` pack (31 :: Unsigned 32)
            shifted = shiftR# (pack (readRegister registers rs1)) shift
            result  = unpack $ replaceFromMsr shifted shift msb
            registers' = writeRegister registers rd result
        SLT  -> (CPUState LoadingInstruction registers', ram)
          where
            result = case complement# (pack (readRegister registers rs1)) `lt#` complement# (pack (readRegister registers rs2)) of
              True -> 1
              False -> 0
            registers' = writeRegister registers rd result
        SLTU -> (CPUState LoadingInstruction registers', ram)
          where
            result = case readRegister registers rs1 < readRegister registers rs2 of
              True -> 1
              False -> 0
            registers' = writeRegister registers rd result
    I format -> case format of
      IArith arith (Word12 im12) rs1 rd -> case arith of
        ADDI  -> (CPUState LoadingInstruction registers', ram)
          where
          result     = readRegister registers rs1 + resize im12
          registers' = writeRegister registers rd result
        SLTI  -> (CPUState LoadingInstruction registers', ram) -- TODO: require check, compare as "two's complement"
          where
            result = case complement# (pack (readRegister registers rs1)) `lt#` complement# (pack (resize im12)) of
              True -> 1
              False -> 0
            registers' = writeRegister registers rd result
        SLTIU -> (CPUState LoadingInstruction registers', ram)
          where
            result = case readRegister registers rs1 < resize im12 of
              True -> 1
              False -> 0
            registers' = writeRegister registers rd result
        XORI  -> (CPUState LoadingInstruction registers', ram)
          where
            result     = unpack $ pack (readRegister registers rs1) `or#` pack (resize im12)
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
            shift = case index# (pack shamt) (size# (pack shamt)) - 1 of
              0 -> fromIntegral $ toInteger# $ pack shamt
              1 -> 0
            result = unpack $ shiftL# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers' rd result
        SRAI -> (CPUState LoadingInstruction registers', ram)
          where
            shift = case index# (pack shamt) (size# (pack shamt)) - 1 of
              0 -> fromIntegral $ toInteger# $ pack shamt
              1 -> 0
            msb     = msb# $ pack (readRegister registers rs1)
            shifted = shiftR# (pack (readRegister registers rs1)) shift
            result  = unpack $ replaceFromMsr shifted shift msb
            registers' = writeRegister registers rd result
        SRLI -> (CPUState LoadingInstruction registers', ram)
          where
            shift = case index# (pack shamt) (size# (pack shamt)) -1 of
              0 -> fromIntegral $ toInteger# $ pack shamt
              1 -> 0
            shifted = shiftR# (pack (readRegister registers rs1)) shift
            result  = unpack $ replaceFromMsr shifted shift 0
            registers' = writeRegister registers rd result
      IJalr jalr (Word12 offset) rs1 rd -> case jalr of -- TODO: require check
        JALR -> undefined
      ILoad load (Word12 offset) rs1 rd -> case load of -- TODO: require check. all ILoad
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
        LBU -> (CPUState LoadingInstruction registers', ram) -- TODO: require check
          where
            ptr = Ptr $ readRegister registers rs1 + resize offset
            Word8 value = readRAM1Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LHU -> (CPUState LoadingInstruction registers', ram)
            where
              ptr = Ptr $ readRegister registers rs1 + resize offset
              Word16 value = readRAM2Byte ram ptr
              result = resize value
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
      SStore store (Word7 offset1) rs2 rs1 (Word5 offset2) -> case store of -- TODO: to WritingMemory
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
            value  = Word32 $ resize $ readRegister registers rs2
            ram'   = writeRAM4Byte ram ptr value
    B format -> case format of
      BBranch br (Word7 offset1) rs2 rs1 (Word5 offset2) -> case br of
        BEQ  -> (CPUState LoadingInstruction registers', ram)
          where
            offset  = unpack $ pack offset1 ++# pack offset2
            pc'     = pc registers
            pc''    = case readRegister registers rs1 == readRegister registers rs2 of
              True  -> addptr pc' (resize offset)
              False -> pc'
            registers' = registers {pc = pc''}
        BNE  -> (CPUState LoadingInstruction registers', ram)
          where
            offset = unpack $ pack offset1 ++# pack offset2
            pc'    = pc registers'
            pc''   = case readRegister registers rs1 == readRegister registers rs2 of
              True -> pc'
              False -> addptr pc' (resize offset)
            registers' = registers {pc = pc''}
        BLT  -> (CPUState LoadingInstruction registers', ram)
          where
            offset = unpack $ pack offset1 ++# pack offset2
            pc'    = pc registers
            pc''   = case complement# (pack (readRegister registers rs1)) `lt#` complement# (pack (readRegister registers rs2)) of
              True  -> addptr pc' (resize offset)
              False -> pc'
            registers' = registers {pc = pc''}
        BGE  -> (CPUState LoadingInstruction registers', ram)
          where
            offset = unpack $ pack offset1 ++# pack offset2
            pc'    = pc registers
            pc''   = case complement# (pack (readRegister registers rs1)) `ge#` complement# (pack (readRegister registers rs2)) of
              True  -> addptr pc' (resize offset)
              False -> pc'
            registers' = registers {pc = pc''}
        BLTU -> (CPUState LoadingInstruction registers', ram)
          where
            offset = unpack $ pack offset1 ++# pack offset2
            pc'    = pc registers
            pc''   = case readRegister registers rs1 < readRegister registers rs2 of
              True  -> addptr pc' (resize offset)
              False -> pc'
            registers' = registers {pc = pc''}
        BGEU -> (CPUState LoadingInstruction registers', ram)
          where
            offset = unpack $ pack offset1 ++# pack offset2
            pc'    = pc registers
            pc''   = case readRegister registers rs1 >= readRegister registers rs2 of
              True  -> addptr pc' (resize offset)
              False -> pc'
            registers' = registers {pc = pc''}
    U format -> case format of
      UArith arith (Word20 im20) rd -> case arith of
        LUI   -> (CPUState LoadingInstruction registers', ram)
          where
            result = unpack $ (shiftL# (pack im20) 12) ++# (0 :: BitVector 12)
            registers' = writeRegister registers rd result
        AUIPC -> (CPUState LoadingInstruction registers', ram)
          where
            shifted = unpack $ (shiftR# ((pack im20) ++# (0 :: BitVector 12)) 12)
            Ptr ad  = pc registers
            result  = shifted + ad
            registers' = writeRegister registers rd result
    J format -> case format of
      JJal jal (Word20 offset) rd -> case jal of
        JAL -> undefined
  -- Outputting _ -> undefined
  -- Halted -> undefined
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


-- isHalted :: CPUState -> Bool
-- isHalted (CPUState Halted _) = True
-- isHalted _                   = False
--
-- output :: CPUState -> Maybe Output
-- output (CPUState (Outputting output) _) = Just output
-- output _                                = Nothing

-- cpuHardware :: (HiddenClockResetEnable dom) => CPUState -> RAM -> Signal dom (Bool, Maybe Output)
-- cpuHardware initialCPUState initialRAM = outputSignal
--   where
--     systemState :: (HiddenClockResetEnable dom) => Signal dom (CPUState, RAM)
--     systemState  = register (initialCPUState, initialRAM) systemState'
--
--     systemState' :: (HiddenClockResetEnable dom) => Signal dom (CPUState, RAM)
--     systemState' = fmap cycle systemState
--
--     getOutput :: (CPUState, RAM) -> (Bool, Maybe Output)
--     getOutput (state, _) = (isHalted state, output state)
--
--     outputSignal :: (HiddenClockResetEnable dom) => Signal dom (Bool, Maybe Output)
--     outputSignal = fmap getOutput systemState'

-- TODO: test
cpuHardware :: (HiddenClockResetEnable dom) => CPUState -> RAM -> Signal dom (Unsigned 32, Unsigned 32, Unsigned 32)
cpuHardware initialCPUState initialRAM = resultSignal
  where
    systemState  :: (HiddenClockResetEnable dom) => Signal dom (CPUState, RAM)
    systemState  = register (initialCPUState, initialRAM) systemState'

    systemState' :: (HiddenClockResetEnable dom) => Signal dom (CPUState, RAM)
    systemState' = fmap cycle systemState

    execute :: (CPUState, RAM) -> (Unsigned 32, Unsigned 32, Unsigned 32)
    execute ((CPUState _ registers), _) = (x0 registers, x1 registers, x2 registers)

    resultSignal :: (HiddenClockResetEnable dom) => Signal dom (Unsigned 32, Unsigned 32, Unsigned 32)
    resultSignal = fmap execute systemState'

defaultCPUState :: CPUState
defaultCPUState = CPUState LoadingInstruction (Registers 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (Ptr 0))


simpleProgram :: Vec 3 Instruction
simpleProgram = I (IArith ADDI (Word12 1) X0 X1) :>
                I (IArith ADDI (Word12 2) X0 X2) :>
                R (RArith ADD X1 X2 X0) :>
                Nil

simpleProgramMem :: Vec 32 Word32
simpleProgramMem = fmap encodeInstruction simpleProgram ++ repeat (Word32 0)

simpleProgramCpu :: HiddenClockResetEnable dom => Signal dom (Unsigned 32, Unsigned 32, Unsigned 32)
simpleProgramCpu = cpuHardware defaultCPUState (RAM simpleProgramMem)

--
-- λ > simpleProgramOutput
-- [(0,0,0),(0,0,0),(2,0,0),(2,0,0),(2,4,0),(2,4,0),(2,4,6),(2,4,6)*** Exception: /Users/mikiyaf/Documents/haskell/Clash/hcpu/riscv-rv32i/src/RV32I.hs:(463,39)-(475,2): Non-exhaustive patterns in case
--
simpleProgramOutput :: [(Unsigned 32, Unsigned 32, Unsigned 32)]
simpleProgramOutput = take 20 $ sample (simpleProgramCpu :: Signal System (Unsigned 32, Unsigned 32, Unsigned 32))

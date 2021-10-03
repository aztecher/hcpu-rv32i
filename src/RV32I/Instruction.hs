{-# LANGUAGE NoImplicitPrelude #-}
module RV32I.Instruction where

import RV32I.Format
import RV32I.Word
import RV32I.Register
import Clash.Prelude (slice, mapM_, print)
import Clash.Sized.BitVector (BitVector, Bit, (++#), size#)
import Clash.Class.BitPack (pack, unpack)
import Clash.Promoted.Nat.Literals as Nat
import Clash.XException (NFDataX)
import Prelude (Show, Eq, (+), (-), ($))
import GHC.Generics (Generic)

-- RV32I instruction
data Instruction
  = R RFormat
  | I IFormat
  | S SFormat
  | B BFormat
  | U UFormat
  | J JFormat
  deriving (Show, Generic, NFDataX, Eq)


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

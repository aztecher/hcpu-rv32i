module RV32I.DSL where

import RV32I.Instruction
import RV32I.Register
import RV32I.Word
import RV32I.Format
import Clash.Prelude (slice)
import Clash.Sized.Signed (Signed)
import Clash.Sized.BitVector (BitVector, (++#))
import Clash.Class.BitPack (pack, unpack)
import Clash.Promoted.Nat.Literals as Nat
import Clash.XException (NFDataX)
import GHC.Generics (Generic)
import Prelude (($))

-- This DSL convert raw opecode to human readable RV32I instruction.

-- R-type Format
--  ADD
add :: Register -> Register -> Register -> Instruction
add rd rs1 rs2 = R (RArith ADD rs2 rs1 rd)
--  SUB
sub :: Register -> Register -> Register -> Instruction
sub rd rs1 rs2 = R (RArith SUB rs2 rs1 rd)
--  AND
and :: Register -> Register -> Register -> Instruction
and rd rs1 rs2 = R (RArith AND rs2 rs1 rd)
--  OR
or :: Register -> Register -> Register -> Instruction
or rd rs1 rs2 = R (RArith OR rs2 rs1 rd)
--  XOR
xor :: Register -> Register -> Register -> Instruction
xor rd rs1 rs2 = R (RArith XOR rs2 rs1 rd)
--  SLL
sll :: Register -> Register -> Register -> Instruction
sll rd rs1 rs2 = R (RArith SLL rs2 rs1 rd)
--  SRA
sra :: Register -> Register -> Register -> Instruction
sra rd rs1 rs2 = R (RArith SRA rs2 rs1 rd)
--  SRL
srl :: Register -> Register -> Register -> Instruction
srl rd rs1 rs2 = R (RArith SRL rs2 rs1 rd)
--  SLT
slt :: Register -> Register -> Register -> Instruction
slt rd rs1 rs2 = R (RArith SLT rs2 rs1 rd)
--  SLTU
sltu :: Register -> Register -> Register -> Instruction
sltu rd rs1 rs2 = R (RArith SLTU rs2 rs1 rd)
-- I-type Format
--  ADDI
addi :: Register -> Register -> Signed 12 -> Instruction
addi rd rs1 imm = I (IArith ADDI (Word12 imm) rs1 rd)
--  SLTI
slti :: Register -> Register -> Signed 12 -> Instruction
slti rd rs1 imm = I (IArith SLTI (Word12 imm) rs1 rd)
--  SLTIU
sltiu :: Register -> Register -> Signed 12 -> Instruction
sltiu rd rs1 imm = I (IArith SLTIU (Word12 imm) rs1 rd)
--  XORI
xori :: Register -> Register -> Signed 12 -> Instruction
xori rd rs1 imm = I (IArith XORI (Word12 imm) rs1 rd)
--  ORI
ori :: Register -> Register -> Signed 12 -> Instruction
ori rd rs1 imm = I (IArith ORI (Word12 imm) rs1 rd)
--  ANDI
andi :: Register -> Register -> Signed 12 -> Instruction
andi rd rs1 imm = I (IArith ANDI (Word12 imm) rs1 rd)
--  SLLI
slli :: Register -> Register -> Signed 5 -> Instruction
slli rd rs1 shamt = I (IShift SLLI (Word5 shamt) rs1 rd)
--  SRAI
srai :: Register -> Register -> Signed 5 -> Instruction
srai rd rs1 shamt = I (IShift SRAI (Word5 shamt) rs1 rd)
--  SRLI
srli :: Register -> Register -> Signed 5 -> Instruction
srli rd rs1 shamt = I (IShift SRLI (Word5 shamt) rs1 rd)
--  JALR
jalr :: Register -> Signed 12 -> Register -> Instruction
jalr rd offset rs1 = I (IJalr JALR (Word12 offset) rs1 rd)
--  LB
lb :: Register -> Signed 12 -> Register -> Instruction
lb rd offset rs1 = I (ILoad LB (Word12 offset) rs1 rd)
--  LH
lh :: Register -> Signed 12 -> Register -> Instruction
lh rd offset rs1 = I (ILoad LH (Word12 offset) rs1 rd)
--  LW
lw :: Register -> Signed 12 -> Register -> Instruction
lw rd offset rs1 = I (ILoad LW (Word12 offset) rs1 rd)
--  LBU
lbu :: Register -> Signed 12 -> Register -> Instruction
lbu rd offset rs1 = I (ILoad LBU (Word12 offset) rs1 rd)
--  LHU
lhu :: Register -> Signed 12 -> Register -> Instruction
lhu rd offset rs1 = I (ILoad LHU (Word12 offset) rs1 rd)
--S-type Format
--  SB
sb :: Register -> Signed 12 -> Register -> Instruction
sb rs2 offset rs1 = S (SStore SB offset7 rs2 rs1 offset5)
  where
       (offset7, offset5) = _make_offsets_of_sformat offset
--  SH
sh :: Register -> Signed 12 -> Register -> Instruction
sh rs2 offset rs1 = S (SStore SH offset7 rs2 rs1 offset5)
  where
       (offset7, offset5) = _make_offsets_of_sformat offset
--  SW
sw :: Register -> Signed 12 -> Register -> Instruction
sw rs2 offset rs1 = S (SStore SW offset7 rs2 rs1 offset5)
  where
       (offset7, offset5) = _make_offsets_of_sformat offset
--B-type Format (RECONSIDER)
--  BEQ
beq :: Register -> Register -> Signed 12 -> Instruction
beq rs1 rs2 offset = B (BBranch BEQ offset7 rs2 rs1 offset5)
  where
      (offset7, offset5) = _make_offsets_of_bfomrat offset
--  BNE
bne :: Register -> Register -> Signed 12 -> Instruction
bne rs1 rs2 offset = B (BBranch BNE offset7 rs2 rs1 offset5)
  where
      (offset7, offset5) = _make_offsets_of_bfomrat offset
--  BLT
blt :: Register -> Register -> Signed 12 -> Instruction
blt rs1 rs2 offset = B (BBranch BLT offset7 rs2 rs1 offset5)
  where
      (offset7, offset5) = _make_offsets_of_bfomrat offset
--  BGE
bge :: Register -> Register -> Signed 12 -> Instruction
bge rs1 rs2 offset = B (BBranch BGE offset7 rs2 rs1 offset5)
  where
      (offset7, offset5) = _make_offsets_of_bfomrat offset
--  BLTU
bltu :: Register -> Register -> Signed 12 -> Instruction
bltu rs1 rs2 offset = B (BBranch BLTU offset7 rs2 rs1 offset5)
  where
      (offset7, offset5) = _make_offsets_of_bfomrat offset
--  BGEU
bgeu :: Register -> Register -> Signed 12 -> Instruction
bgeu rs1 rs2 offset = B (BBranch BGEU offset7 rs2 rs1 offset5)
  where
      (offset7, offset5) = _make_offsets_of_bfomrat offset
--U-type Format
--  LUI
lui :: Register -> Signed 20 -> Instruction
lui rd imm = U (UArith LUI (Word20 imm) rd)
--  AUIPC
auipc :: Register -> Signed 20 -> Instruction
auipc rd imm = U (UArith AUIPC (Word20 imm) rd)
--J-type Format
--  JAL
jal :: Register -> Signed 20 -> Instruction
jal rd offset = J (JJal JAL (Word20 offset) rd)


_slice_to_word7 :: Signed 12 -> Word7
_slice_to_word7 num
  = Word7 $ unpack (slice Nat.d11 Nat.d5 (pack num))

_slice_to_word5 :: Signed 12 -> Word5
_slice_to_word5 num
  = Word5 $ unpack (slice Nat.d4 Nat.d0 (pack num))

_make_offsets_of_sformat :: Signed 12 -> (Word7, Word5)
_make_offsets_of_sformat offset =
  (Word7 $ unpack offset7, Word5 $ unpack offset5)
  where
    offset7 = slice Nat.d11 Nat.d5 (pack offset)
    offset5 = slice Nat.d4 Nat.d0 (pack offset)


_make_offsets_of_bfomrat :: Signed 12 -> (Word7, Word5)
_make_offsets_of_bfomrat imm =
  (Word7 $ unpack offset7, Word5 $ unpack offset5)
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

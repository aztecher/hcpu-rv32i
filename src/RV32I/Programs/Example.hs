module RV32I.Programs.Example where

import Clash.Prelude (slice)
import Clash.Promoted.Nat.Literals as Nat
import Clash.Class.BitPack (pack, unpack)
import Clash.Sized.BitVector (BitVector, (++#))
import Clash.Sized.Signed (Signed)
import Clash.Sized.Vector (Vec (Nil, (:>)))
import RV32I.RV32I

-- Test ADDI/ADD/JAL
addImm :: Vec 4 Instruction
addImm =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> R (RArith ADD S1 S2 S0)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S3)
    :> Nil

-- Test ADDI/ADD/JAL
addPlusMinus :: Vec 4 Instruction
addPlusMinus =
  I (IArith ADDI (Word12 (-1 :: Signed 12)) S2 S3)
    :> I (IArith ADDI (Word12 2) S2 S4)
    :> R (RArith ADD S3 S4 S2)
    --pesudo stop
    :> J (JJal JAL (Word20 3) S5)
    :> Nil

-- Test ADDI/SUB/JAL
subImm :: Vec 4 Instruction
subImm =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> R (RArith SUB S2 S1 S3)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test ADDI/AND
-- 0001(1) AND 0001(1) = 0001(1)
andImm :: Vec 3 Instruction
andImm =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ANDI (Word12 1) S1 S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test ADDI/ANDI
-- 0011(3) AND 0101(5) = 0001(1)
andDiff :: Vec 4 Instruction
andDiff =
  I (IArith ADDI (Word12 3) Zero S1)
  :> I (IArith ADDI (Word12 5) Zero S2)
  :> R (RArith AND S1 S2 S3)
  -- pesudo stop
  :> J (JJal JAL (Word20 3) S4)
  :> Nil

-- Test ADDI/ORI
-- 0001(1) OR 0001(1) = 0001(1)
orImm :: Vec 3 Instruction
orImm =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ORI (Word12 1) S1 S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test ADDI/OR
-- 0011(3) OR 0101(5) = 0111(7)
orDiff :: Vec 4 Instruction
orDiff =
  I (IArith ADDI (Word12 3) Zero S1)
    :> I (IArith ADDI (Word12 5) Zero S2)
    :> R (RArith OR S1 S2 S3)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test ADDI/XOR
-- 0001(1) XOR 0001(1) = 0000(0)
xorImm :: Vec 3 Instruction
xorImm =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith XORI (Word12 1) S1 S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test ADDI/XORI
-- 0011(3) OR 0101(5) = 0110(6)
xorDiff :: Vec 4 Instruction
xorDiff =
  I (IArith ADDI (Word12 3) Zero S1)
    :> I (IArith ADDI (Word12 5) Zero S2)
    :> R (RArith XOR S1 S2 S3)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test SLL
-- 11 -(4bit left shift)-> 1_1000 (3 * 2 * 2 * 2 = 48)
sll =
  I (IArith ADDI (Word12 3) Zero S1)
    :> I (IArith ADDI (Word12 4) Zero S2)
    :> R (RArith SLL S2 S1 S3)
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test SLLI Msb is '0'
slliMsbZero =
  I (IArith ADDI (Word12 3) Zero S1)
    :> I (IShift SLLI (Word5 4) S1 S2)
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test SLLI Msb is '1' (actually, unconsidered case)
slliMsbOne =
  I (IArith ADDI (Word12 3) Zero S1)
    :> I (IShift SLLI (Word5 (-1 :: Signed 5)) S1 S2)
    :> J (JJal JAL (Word20 2) S3)
    :> Nil


-- Test SRA
-- 1024 / 2 / 2 / 2 / 2 / 2 = 32
sraPlus =
  I (IArith ADDI (Word12 1024) Zero S1)
    :> I (IArith ADDI (Word12 5) Zero S2)
    :> R (RArith SRA S2 S1 S3)
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test SRA
-- 1024 / 2 / 2 / 2 / 2 = -32
sraMinus =
  I (IArith ADDI (Word12 (-1024 :: Signed 12)) Zero S1)
    :> I (IArith ADDI (Word12 5) Zero S2)
    :> R (RArith SRA S2 S1 S3)
    :> J (JJal JAL (Word20 3) S4)
    :> Nil


-- Test SRAI
-- 1024 / 2 / 2 / 2 / 2 = 32
sraiPlus =
  I (IArith ADDI (Word12 1024) Zero S1)
    :> I (IShift SRAI (Word5 5) S1 S2)
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test SRAI
-- minus shift value don't affect (unexpected case)
-- S1 = S2 = -1024
sraiMinusShift =
  I (IArith ADDI (Word12 1024) Zero S1)
    :> I (IShift SRAI (Word5 (-5 :: Signed 5)) S1 S2)
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test SRL
-- 1024 / 2 / 2 / 2 / 2 = 32
srlPlus =
  I (IArith ADDI (Word12 1024) Zero S1)
    :> I (IArith ADDI (Word12 5) Zero S2)
    :> R (RArith SRL S2 S1 S3)
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test SRL minus value
-- minus value is strange behavior
-- -1024 : 1111_1111_1111_1111_1111_1100_0000_0000
-- right shift by SRL, storange value (by it's definition)
-- 134217696 ::  0000_0111_1111_1111_1111_1111_1110_0000
-- SRL will not be used in Minus value?
srlMinus =
  I (IArith ADDI (Word12 (-1024 :: Signed 12)) Zero S1)
    :> I (IArith ADDI (Word12 5) Zero S2)
    :> R (RArith SRL S2 S1 S3)
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test SRLI plus value
-- 1024 / 2 / 2 / 2 / 2 / 2 = 32
srliPlus =
  I (IArith ADDI (Word12 1024) Zero S1)
    :> I (IShift SRLI (Word5 5) S1 S2)
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- minus shift value don't affect (unexpected case)
-- S1 = S2 = -1024
srliMinusShift =
  I (IArith ADDI (Word12 1024) Zero S1)
    :> I (IShift SRLI (Word5 (-5 :: Signed 5)) S1 S2)
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test JALR
-- pc = S1(1) + offset(1) = 2 = 0010 (even number)
-- rd = 2(pc of JALR) + 1 (next inst) = 3
jalr =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IJalr JALR (Word12 1) S1 S2)
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test SLTI lt
-- 0001(1) < 0010(2) then S2(1)
sltImmLt :: Vec 3 Instruction
sltImmLt =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith SLTI (Word12 2) S1 S2)
  -- pesudo stop
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test SLTI geq
-- 0001(1) LT 1110(-2) then S2(0)
sltImmGt :: Vec 3 Instruction
sltImmGt =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith SLTI (Word12 (-2 :: Signed 12)) S1 S2)
    --pesudo stop
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test SLT lt
sltDiffLt :: Vec 4 Instruction
sltDiffLt =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> R (RArith SLT S2 S1 S3)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test SLT geq
sltDiffGt :: Vec 4 Instruction
sltDiffGt =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 (-2 :: Signed 12)) Zero S2)
    :> R (RArith SLT S2 S1 S3)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test SLTIU lt
sltiuLt :: Vec 3 Instruction
sltiuLt =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith SLTIU (Word12 15) S1 S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test SLTIU geq
sltiuGeq :: Vec 3 Instruction
sltiuGeq =
  I (IArith ADDI (Word12 8) Zero S1)
    :> I (IArith SLTIU (Word12 8) S1 S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 2) S3)
    :> Nil

-- Test SLTU lt
sltuDiffLt :: Vec 4 Instruction
sltuDiffLt =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 15) Zero S2)
    :> R (RArith SLTU S2 S1 S3)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test SLTU geq
sltuDiffGt :: Vec 4 Instruction
sltuDiffGt =
  I (IArith ADDI (Word12 12) Zero S1)
    :> I (IArith ADDI (Word12 8) Zero S2)
    :> R (RArith SLTU S2 S1 S3)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S4)
    :> Nil

-- Test JALR


-- Test SB/LB
sblb =
  I (IArith ADDI (Word12 1) Zero S1)
    :> S (SStore SB (_slice_to_word7 offset) S1 Zero (_slice_to_word5 offset))
    :> I (ILoad LB (Word12 offset) Zero S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S3)
    :> Nil
  where
    offset = 900

-- Test SB/LB over one Byte
--   1_0000_0000 -> store lowest byte to memry -> 0000
--   and load it to register -> 0000
sblbOverByte =
  I (IArith ADDI (Word12 256) Zero S1)
    :> S (SStore SB (_slice_to_word7 offset) S1 Zero (_slice_to_word5 offset))
    :> I (ILoad LB (Word12 offset) Zero S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S3)
    :> Nil
  where
    offset = 900

-- Test SH/LH
--   0000_0001_0000_0000 -> store lowest 2byte to memry -> 0000_0001_0000_0000
--   and load it to register -> 0000_0001_0000_0000
shlh =
  I (IArith ADDI (Word12 256) Zero S1)
    :> S (SStore SH (_slice_to_word7 offset) S1 Zero (_slice_to_word5 offset))
    :> I (ILoad LH (Word12 offset) Zero S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S3)
    :> Nil
  where
    offset = 900

-- Test SB/LB over 2 Byte
--   1_0000_0000_0000_0000 -> store lowest 2byte to memry -> 0000_0000_0000_0000
--   and load it to register -> 0000_0000_0000_0000
--   WARNING: Word12 65536 -> 0000_0000_0000, so it can't represents 65536
shlhOver2Byte =
  I (IArith ADDI (Word12 65536) Zero S1)
    :> S (SStore SH (_slice_to_word7 offset) S1 Zero (_slice_to_word5 offset))
    :> I (ILoad LH (Word12 offset) Zero S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S3)
    :> Nil
  where
    offset = 900


-- Test SW/LW
swlw =
  I (IArith ADDI (Word12 256) Zero S1)
    :> S (SStore SW (_slice_to_word7 offset) S1 Zero (_slice_to_word5 offset))
    :> I (ILoad LH (Word12 offset) Zero S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S3)
    :> Nil
  where
    offset = 900

-- Test SB/LBU
sblbu =
  I (IArith ADDI (Word12 1) Zero S1)
    :> S (SStore SB (_slice_to_word7 offset) S1 Zero (_slice_to_word5 offset))
    :> I (ILoad LBU (Word12 offset) Zero S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S3)
    :> Nil
  where
    offset = 900

-- Test SB/LBU MSB Bit is '1'
--   1000(8) -> 0000_0000_0000_0000_0000_0000_0000_1000
sblbuMsbOne =
  I (IArith ADDI (Word12 8) Zero S1)
    :> S (SStore SB (_slice_to_word7 offset) S1 Zero (_slice_to_word5 offset))
    :> I (ILoad LBU (Word12 offset) Zero S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S3)
    :> Nil
  where
    offset = 900

-- Test SH/LHU
shlhu =
  I (IArith ADDI (Word12 1) Zero S1)
    :> S (SStore SH (_slice_to_word7 offset) S1 Zero (_slice_to_word5 offset))
    :> I (ILoad LHU (Word12 offset) Zero S2)
    -- pesudo stop
    :> J (JJal JAL (Word20 3) S3)
    :> Nil
  where
    offset = 900

-- Test SB/LBU MSB Bit is '1'
-- 1000_0000(128) -> 0000_0000_0000_0000_0000_0000_1000_0000
shlhuMsbOne =
  I (IArith ADDI (Word12 128) Zero S1)
    :> S (SStore SH (_slice_to_word7 offset) S1 Zero (_slice_to_word5 offset))
    :> I (ILoad LHU (Word12 offset) Zero S2)
    :> J (JJal JAL (Word20 3) S3)
    :> Nil
  where
    offset = 900

-- RECONSIDER: BFormat require imm value is even value?
-- (more accurately, it represent the address space,
--  and, address space will 4byte unit)

-- Test BEQ equal pattern
beq =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 1) Zero S2)
    :> B (BBranch BEQ offset7 S2 S1 offset5)
    :> J (JJal JAL (Word20 3) S3)
    :> I (IArith ADDI (Word12 0) Zero S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BEQ not equal pattern
beqNg =
  I (IArith ADDI (Word12 2) Zero S1)
    :> I (IArith ADDI (Word12 1) Zero S2)
    :> B (BBranch BEQ offset7 S2 S1 offset5)
    :> J (JJal JAL (Word20 3) S3)
    :> I (IArith ADDI (Word12 0) Zero S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BNE not equal pattern
bne =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 1) Zero S2)
    :> B (BBranch BNE offset7 S2 S1 offset5)
    :> J (JJal JAL (Word20 3) S3)
    :> I (IArith ADDI (Word12 0) Zero S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BNE equal pattern
bneNg =
  I (IArith ADDI (Word12 0) Zero S1)
    :> I (IArith ADDI (Word12 1) Zero S2)
    :> B (BBranch BNE offset7 S2 S1 offset5)
    :> J (JJal JAL (Word20 3) S3)
    :> I (IArith ADDI (Word12 0) Zero S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BLT lt and plus pattern
bltLtPlus =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> B (BBranch BLT offset7 S2 S1 offset5)
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BLT lt and minus pattern
bltLtMinus =
  I (IArith ADDI (Word12 (-2 :: Signed 12)) Zero S1)
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) Zero S2)
    :> B (BBranch BLT (offset7) S2 S1 (offset5))
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BLT geq and plus pattern
bltGeqPlus =
  I (IArith ADDI (Word12 3) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> B (BBranch BLT (offset7) S2 S1 (offset5))
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BLT geq and minus pattern
bltGeqMinus =
  I (IArith ADDI (Word12 (-1 :: Signed 12)) Zero S1)
    :> I (IArith ADDI (Word12 (-2 :: Signed 12)) Zero S2)
    :> B (BBranch BLT (offset7) S2 S1 (offset5))
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BGE
bgeGePlus =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 1) Zero S2)
    :> B (BBranch BGE offset7 S2 S1 offset5)
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

bgeGeMinus =
  I (IArith ADDI (Word12 (-1 :: Signed 12)) Zero S1)
    :> I (IArith ADDI (Word12 (-2 :: Signed 12)) Zero S2)
    :> B (BBranch BGE offset7 S2 S1 offset5)
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

bgeLtPlus =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> B (BBranch BGE offset7 S2 S1 offset5)
    :> I (IArith ADDI (Word12 (1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

bgeLtMinus =
  I (IArith ADDI (Word12 (-2 :: Signed 12)) Zero S1)
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) Zero S2)
    :> B (BBranch BGE offset7 S2 S1 offset5)
    :> I (IArith ADDI (Word12 (1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset


-- Test BLTU lt
bltult =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> B (BBranch BLTU offset7 S2 S1 offset5)
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BLTU ge
bltuge =
  I (IArith ADDI (Word12 2) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> B (BBranch BLTU offset7 S2 S1 offset5)
    :> I (IArith ADDI (Word12 (-1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BGEU ge
bgeuge =
  I (IArith ADDI (Word12 2) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> B (BBranch BGEU offset7 S2 S1 offset5)
    :> I (IArith ADDI (Word12 (1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test BGEU lt
bgeult =
  I (IArith ADDI (Word12 1) Zero S1)
    :> I (IArith ADDI (Word12 2) Zero S2)
    :> B (BBranch BGEU offset7 S2 S1 offset5)
    :> I (IArith ADDI (Word12 (1 :: Signed 12)) S1 S1)
    :> J (JJal JAL (Word20 2) S3)
    :> J (JJal JAL (Word20 5) S4)
    :> Nil
  where
    offset = 2
    (offset7, offset5) = _make_offsets_of_bfomrat offset

-- Test LUI plus value
-- 5 * 2 ^ 12 = 5 * 4096 = 20480
luiPlus =
  U (UArith LUI (Word20 5) S1)
    :> J (JJal JAL (Word20 1) S2)
    :> Nil

-- Test LUI minus value
-- -5 ( 2 ^ 12 = -5 * 4096 = -20480
luiMinus =
  U (UArith LUI (Word20 (-5 :: Signed 20)) S1)
    :> J (JJal JAL (Word20 1) S2)
    :> Nil

-- Test AUIPC
auipc =
  U (UArith AUIPC (Word20 5) S1)
    :> J (JJal JAL (Word20 1) S2)
    :> Nil

_slice_to_word7 :: Signed 12 -> Word7
_slice_to_word7 num
  = Word7 $ unpack (slice Nat.d11 Nat.d5 (pack num))

_slice_to_word5 :: Signed 12 -> Word5
_slice_to_word5 num
  = Word5 $ unpack (slice Nat.d4 Nat.d0 (pack num))

-- _make_offset_of_bfomrat :: Signed 12 -> Signed 13
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


{-# LANGUAGE NoImplicitPrelude #-}
module RV32I.CPU where

import RV32I.RAM
import RV32I.Register
import RV32I.Format
import RV32I.Word
import RV32I.Instruction
import Clash.Prelude (slice, mapM_, print)
import Clash.Signal
  (Signal, System, HiddenClockResetEnable, register, sample, exposeClockResetEnable)
import Clash.Sized.BitVector (BitVector, Bit, (++#), size#)
import Clash.Sized.Internal.BitVector
  (and#, or#, xor#, toInteger#, fromInteger#, shiftL#, shiftR#, index#,
   msb#, replaceBit#, lt#, gt#, ge#, complement#, neq#)
import Clash.Sized.Vector (Vec((:>), Nil), (!!), (++), repeat )
import Clash.Sized.Signed (Signed)
import Clash.Sized.Unsigned (Unsigned)
import Clash.Class.BitPack (pack, unpack)
import Clash.Class.Resize (resize)
import Clash.Promoted.Nat.Literals as Nat
import Clash.XException (NFDataX)
import GHC.Generics (Generic)
import Prelude
  (($), (+), (-), (*), (==), (<), (.),
   undefined, take, fmap, otherwise, fromIntegral,
   Show, Eq, IO, Int, Bool(..))
import GHC.Stack (HasCallStack)
import Text.Pretty.Simple (pPrint)


-- CPU State
data CPUActivity
  = LoadingInstruction
  | IncrementProgramCounter
  | ExecutingInstruction Instruction deriving (Show, Generic, NFDataX)
  -- -- | Halted

data CPUState = CPUState CPUActivity Registers deriving (Show, Generic, NFDataX)


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


cycle :: HasCallStack => (CPUState, RAM) -> (CPUState, RAM)
cycle (CPUState activity registers, ram) = case activity of
  LoadingInstruction -> (CPUState activity' registers, ram)
  IncrementProgramCounter -> (CPUState LoadingInstruction registers', ram)
  ExecutingInstruction instruction -> case instruction of
    R format -> case format of
      RArith rarith rs2 rs1 rd -> case rarith of
        ADD  -> (CPUState IncrementProgramCounter registers', ram)
          where
          result     = readRegister registers rs1 + readRegister registers rs2
          registers' = writeRegister registers rd result
        SUB  -> (CPUState IncrementProgramCounter registers', ram)
          where
            result     = readRegister registers rs1 - readRegister registers rs2
            registers' = writeRegister registers rd result
        AND  -> (CPUState IncrementProgramCounter registers', ram)
          where
            result     = unpack $ pack (readRegister registers rs1) `and#` pack (readRegister registers rs2)
            registers' = writeRegister registers rd result
        OR   -> (CPUState IncrementProgramCounter registers', ram)
          where
            result     = unpack $ pack (readRegister registers rs1) `or#` pack (readRegister registers rs2)
            registers' = writeRegister registers rd result
        XOR  -> (CPUState IncrementProgramCounter registers', ram)
          where
            result     = unpack $ pack (readRegister registers rs1) `xor#` pack (readRegister registers rs2)
            registers' = writeRegister registers rd result
        SLL  -> (CPUState IncrementProgramCounter registers', ram)
          where
            -- shift value is determined by lower 5 bit of rs2
            -- so, you can get this by 'and' with 0...01_1111
            shift = fromIntegral $ toInteger# $ pack (readRegister registers rs2) `and#` pack (31 :: Signed 32)
            result = unpack $ shiftL# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers rd result
        SRA  -> (CPUState IncrementProgramCounter registers', ram)
          where
            msb     = msb# $ pack (readRegister registers rs1)
            shift   = fromIntegral $ toInteger# $ pack (readRegister registers rs2) `and#` pack (31 :: Signed 32)
            shifted = shiftR# (pack (readRegister registers rs1)) shift
            result  = unpack $ replaceFromMsr shifted shift msb
            registers' = writeRegister registers rd result
        SRL -> (CPUState IncrementProgramCounter registers', ram)
          where
            shift = fromIntegral $ toInteger# $ pack (readRegister registers rs2) `and#` pack (31 :: Signed 32)
            result = unpack $ shiftR# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers rd result
        SLT  -> (CPUState IncrementProgramCounter registers', ram)
          where
            calcmsb = msb# $
              ((pack (readRegister registers rs1)) - (pack (readRegister registers rs2)))
            result = case calcmsb of
              (1 :: Bit) -> 1 -- the 'lt' case
              (0 :: Bit) -> 0 -- the 'geq' case
            registers' = writeRegister registers rd result
        SLTU -> (CPUState IncrementProgramCounter registers', ram)
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
        ADDI  -> (CPUState IncrementProgramCounter registers', ram)
          where
          result     = readRegister registers rs1 + resize im12
          registers' = writeRegister registers rd result
        SLTI  -> (CPUState IncrementProgramCounter registers', ram)
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
        SLTIU -> (CPUState IncrementProgramCounter registers', ram)
          where
            urs1 = unpack (pack (readRegister registers rs1)) :: Unsigned 32
            urs2 = unpack (pack (resize im12)) :: Unsigned 32
            result = case urs1 < urs2 of
              True -> 1
              False -> 0
            registers' = writeRegister registers rd result
        XORI  -> (CPUState IncrementProgramCounter registers', ram)
          where
            result     = unpack $ pack (readRegister registers rs1) `xor#` pack (resize im12)
            registers' = writeRegister registers rd result
        ORI   -> (CPUState IncrementProgramCounter registers', ram)
          where
            result = unpack $ pack (readRegister registers rs1) `or#` pack (resize im12)
            registers' = writeRegister registers rd result
        ANDI  -> (CPUState IncrementProgramCounter registers', ram)
          where
            result = unpack $ pack (readRegister registers rs1) `and#` pack (resize im12)
            registers' = writeRegister registers rd result
      IShift shift (Word5 shamt) rs1 rd -> case shift of
        SLLI -> (CPUState IncrementProgramCounter registers', ram)
          where
            shift = case index# (pack shamt) (size# (pack shamt) - 1) of
              0 -> fromIntegral $ toInteger# $ pack shamt
              1 -> 0 -- This case is unexpected in RV32I
            result = unpack $ shiftL# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers rd result
        SRAI -> (CPUState IncrementProgramCounter registers', ram)
          where
            shift = case index# (pack shamt) (size# (pack shamt) - 1) of
              0 -> fromIntegral $ toInteger# $ pack shamt
              1 -> 0 -- This case is unexpected in RV32I
            msb     = msb# $ pack (readRegister registers rs1)
            shifted = shiftR# (pack (readRegister registers rs1)) shift
            result  = unpack $ replaceFromMsr shifted shift msb
            registers' = writeRegister registers rd result
        SRLI -> (CPUState IncrementProgramCounter registers', ram)
          where
            shift = case index# (pack shamt) (size# (pack shamt) - 1) of
              0 -> fromIntegral $ toInteger# $ pack shamt
              1 -> 0
            result = unpack $ shiftR# (pack (readRegister registers rs1)) shift
            registers' = writeRegister registers rd result
      IJalr jalr (Word12 offset) rs1 rd -> case jalr of
        JALR -> (CPUState nActivity registers'', ram)
          where
            Ptr cur = pc registers
            Ptr naddr = increment (pc registers)
            registers' = writeRegister registers rd (offsetToSigned32 naddr)
            -- not replace , mask
            pc' = Ptr $ signed32ToOffset $ unpack $
              replaceBit#
                (pack (readRegister registers rs1 + resize offset))
                0
                0
            registers'' = registers' { pc = pc' }
            nLoadedWord = readRAM ram (pc registers'')
            nActivity   = ExecutingInstruction (decodeInstruction nLoadedWord)
      ILoad load (Word12 offset) rs1 rd -> case load of
        LB  -> (CPUState IncrementProgramCounter registers', ram)
          where
            ptr = Ptr $ signed32ToOffset $ readRegister registers rs1 + resize offset
            Word8 value = readRAM1Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LH  -> (CPUState IncrementProgramCounter registers', ram)
          where
            ptr = Ptr $ signed32ToOffset $ readRegister registers rs1 + resize offset
            Word16 value = readRAM2Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LW  -> (CPUState IncrementProgramCounter registers', ram)
          where
            ptr = Ptr $ signed32ToOffset $ readRegister registers rs1 + resize offset
            Word32 value = readRAM4Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LBU -> (CPUState IncrementProgramCounter registers', ram)
          where
            zeroExpand :: Signed 8 -> Signed 32
            zeroExpand num = unpack $ (0 :: BitVector 24) ++# (pack num)
            ptr = Ptr $ signed32ToOffset $ readRegister registers rs1 + resize offset
            Word8 value = readRAM1Byte ram ptr
            result = zeroExpand value
            registers' = writeRegister registers rd result
        LHU -> (CPUState IncrementProgramCounter registers', ram)
            where
              zeroExpand :: Signed 16 -> Signed 32
              zeroExpand num = unpack $ (0 :: BitVector 16) ++# (pack num)
              ptr = Ptr $ signed32ToOffset $ readRegister registers rs1 + resize offset
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
        SB -> (CPUState IncrementProgramCounter registers, ram')
          where
            offset = unpack $ pack offset1 ++# pack offset2
            ptr    = Ptr $ signed32ToOffset $ readRegister registers rs1 + resize offset
            value  = Word8 $ resize $ readRegister registers rs2
            ram'   = writeRAM1Byte ram ptr value
        SH -> (CPUState IncrementProgramCounter registers, ram')
          where
            offset = unpack $ pack offset1 ++# pack offset2
            ptr    = Ptr $ signed32ToOffset $ readRegister registers rs1 + resize offset
            value  = Word16 $ resize $ readRegister registers rs2
            ram'   = writeRAM2Byte ram ptr value
        SW -> (CPUState IncrementProgramCounter registers, ram')
          where
            offset = unpack $ pack offset1 ++# pack offset2
            ptr    = Ptr $ signed32ToOffset $ readRegister registers rs1 + resize offset
            value  = Word32 $ readRegister registers rs2
            ram'   = writeRAM4Byte ram ptr value
    B format -> case format of
      BBranch br (Word7 offset1) rs2 rs1 (Word5 offset2) -> case br of
        -- BEQ -> (CPUState nActivity registers', ram)
        BEQ -> (nextstate, ram)
          where
            offset  = unpack (bformat_offset offset1 offset2)
            pc'     = pc registers
            nextstate = case readRegister registers rs1 == readRegister registers rs2 of
              True -> let pc'' = addptr pc' (resize offset)
                          registers' = registers {pc = pc''}
                          nLoadedWord = readRAM ram (pc registers')
                          inst = decodeInstruction nLoadedWord
                      in CPUState (ExecutingInstruction inst) registers'
              False -> let registers' = registers {pc = pc'}
                       in CPUState IncrementProgramCounter registers'
        BNE -> (nextstate, ram)
          where
            offset  = unpack (bformat_offset offset1 offset2)
            pc'     = pc registers
            nextstate = case readRegister registers rs1 == readRegister registers rs2 of
              True -> let registers' = registers {pc = pc'}
                      in CPUState IncrementProgramCounter registers'
              False -> let pc'' = addptr pc' (resize offset)
                           registers' = registers {pc = pc''}
                           nLoadedWord = readRAM ram (pc registers')
                           inst = decodeInstruction nLoadedWord
                       in CPUState (ExecutingInstruction inst) registers'
        BLT  -> (nextstate, ram)
          where
            offset = unpack (bformat_offset offset1 offset2)
            pc'    = pc registers
            calcmsb = msb# ((pack (readRegister registers rs1)) - (pack (readRegister registers rs2)))
            nextstate = case calcmsb of
              (1 :: Bit) -> let pc'' = addptr pc' (resize offset)
                                registers' = registers {pc = pc''}
                                nLoadedWord = readRAM ram (pc registers')
                                inst = decodeInstruction nLoadedWord
                            in CPUState (ExecutingInstruction inst) registers'
              (0 :: Bit) -> let registers' = registers {pc = pc'}
                            in CPUState IncrementProgramCounter registers'
        BGE -> (nextstate, ram)
          where
            offset = unpack (bformat_offset offset1 offset2)
            pc'    = pc registers
            calcmsb = msb# ((pack (readRegister registers rs1)) - (pack (readRegister registers rs2)))
            nextstate = case calcmsb of
              (1 :: Bit) -> let registers' = registers {pc = pc'}
                            in CPUState IncrementProgramCounter registers'
              (0 :: Bit) -> let pc'' = addptr pc' (resize offset)
                                registers' = registers {pc = pc''}
                                nLoadedWord = readRAM ram (pc registers')
                                inst = decodeInstruction nLoadedWord
                            in CPUState (ExecutingInstruction inst) registers'
        BLTU -> (nextstate, ram)
          where
            -- Read register value of Signed 32 and convert it to Unsigned
            urs1 = unpack (pack (readRegister registers rs1)) :: Unsigned 32
            urs2 = unpack (pack (readRegister registers rs2)) :: Unsigned 32
            offset = unpack (bformat_offset offset1 offset2)
            pc'    = pc registers
            nextstate = case urs1 < urs2 of
              True -> let pc'' = addptr pc' (resize offset)
                          registers' = registers {pc = pc''}
                          nLoadedWord = readRAM ram (pc registers')
                          inst = decodeInstruction nLoadedWord
                      in CPUState (ExecutingInstruction inst) registers'
              False -> let registers' = registers {pc = pc'}
                       in CPUState IncrementProgramCounter registers'
        BGEU -> (nextstate, ram)
          where
            -- Read register value of Signed 32 and convert it to Unsigned
            urs1 = unpack (pack (readRegister registers rs1)) :: Unsigned 32
            urs2 = unpack (pack (readRegister registers rs2)) :: Unsigned 32
            offset = unpack (bformat_offset offset1 offset2)
            pc'    = pc registers
            nextstate = case urs1 < urs2 of
              True  -> let registers' = registers {pc = pc'}
                      in CPUState IncrementProgramCounter registers'
              False -> let pc'' = addptr pc' (resize offset)
                           registers' = registers {pc = pc''}
                           nLoadedWord = readRAM ram (pc registers')
                           inst = decodeInstruction nLoadedWord
                       in CPUState (ExecutingInstruction inst) registers'
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
        LUI   -> (CPUState IncrementProgramCounter registers', ram)
          where
            result = unpack $ shiftL# (resize $ pack im20) 12
            registers' = writeRegister registers rd result
        AUIPC -> (CPUState IncrementProgramCounter registers', ram)
          where
            shifted = unpack $ shiftL# (resize $ pack im20) 12
            Ptr ad  = pc registers
            result  = shifted + (offsetToSigned32 ad)
            registers' = writeRegister registers rd result
    J format -> case format of
      JJal jal (Word20 offset) rd -> case jal of
        JAL -> (CPUState nActivity registers'', ram)
          where
            Ptr curr = pc registers
            Ptr naddr = increment (pc registers)
            registers' = writeRegister registers rd (offsetToSigned32 naddr)
            pc' = Ptr $ signed32ToOffset $ (offsetToSigned32 curr) + (resize offset)
            -- Check
            registers'' = registers' { pc = pc' }
            nLoadedWord = readRAM ram (pc registers'')
            nActivity = ExecutingInstruction (decodeInstruction nLoadedWord)
  where
    -- For Loading Instruction
    loadedWord = readRAM ram (pc registers)
    activity'  = ExecutingInstruction (decodeInstruction loadedWord)
    -- For IncrementProgramCounter
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
initCPUState = zeroRegisterCPU (Ptr . Offset $ 0)

zeroRegisterCPU :: Ptr -> CPUState
zeroRegisterCPU ptr = CPUState LoadingInstruction (Registers 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ptr)

-- TODO
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
    li   rd  imm        = addi rd T6 imm
    blt  rs1 rs2 offset = B (BBranch BLT (Word7 (b_offset_12_10_5 offset)) rs2 rs1 (Word5 (b_offset_4_1_11 offset)))
    mv   rd  rs1        = addi rd rs1 0
    jal  rd  offset     = J (JJal JAL (Word20 offset) rd)
    jalr rd  offset rs1 = I (IJalr JALR (Word12 offset) rs1 rd)
    -- j    offset         = jal Zero offset
    j    offset         = jal Zero offset
    ret                 = jalr Zero 1 RA -- TODO: Check / 1ずれる? -> address problem (want to 17 -> go to 16)
    -- Fib Program
    -- > ちょっとおかしいかもしれない。確認jmp周り
    fibProgram =
      -- start Fibonacci function from here
         addi SP SP (-10) -- pc = 1
      :> sw   RA 1  SP
      :> sw   S0 2  SP
      :> sw   S1 3  SP
      :> addi S0 SP 10
      :> sw   A0 (-1) S0
      :> lw   A4 (-1) S0
      :> li   A5 1
      :> blt  A5 A4 4
      :> addi T6 T6 0 -- pc = 10( thunk because of Bformat even)
      :> li   A5 1
      :> j    26
      :> lw   A5 (-1) S0 -- pc = 13 / Case: blt A5 < A4
      :> addi A5 A5 (-1)
      :> mv   A0 A5
      :> jal  RA 1
      :> addi T6 T6 0 -- pc = 17( thunk because of Bformat even)
      :> mv   S1 A0 -- pc = 18
      :> lw   A5 (-1) S0
      :> addi A5 A5 (-2)
      :> addi T6 T6 0 -- pc = 21( thunk because of Bformat even)
      :> mv   A0 A5 -- pc = 22
      :> jal  RA 1
      :> mv   A5 A0 -- pc = 24
      :> add  A5 S1 A5
      :> mv   A0 A5 -- pc = 26
      :> lw   RA 1 SP
      :> lw   S0 2 SP
      :> lw   S1 3 SP
      :> addi SP SP 10
      :> ret          -- pc = 31
      -- start main function
      :> addi SP SP 1000 -- PC 32 (= from init Ptr 31)
      :> addi SP SP (-10)
      :> sw   RA 1  SP -- Need to Debug (MEMORY ADDRESS)
      :> sw   S0 2  SP
      :> addi S0 SP 10
      :> li   A0 3 -- fib(n=10)
      :> jal  RA 1 -- jamp to Fibonacci
      :> j 37  -- jamp here
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

programCpu :: HiddenClockResetEnable dom => Signal dom Registers
programCpu = cpuHardware (zeroRegisterCPU (Ptr (Offset $ 32 * 32))) (RAM (programMem ++ repeat (Word32 0)))

-- You can play code in RV32I.RV32I.Programs.Example like follow
-- >>> Prelude.take 10 $ sample (cpu initCPUState (programmedRAM addImm) :: Signal System Registers)
cpu :: HiddenClockResetEnable dom => CPUState -> RAM -> Signal dom Registers
cpu = cpuHardware


programResult :: [Registers]
programResult = take 100 $ sample (programCpu :: Signal System Registers)

programOutput :: IO ()
programOutput = mapM_ print programResult


takeInstruction inst n = pPrint $ Prelude.take n $ sample (cpu initCPUState (programmedRAM inst) :: Signal System Registers)

testInst inst = takeInstruction inst 50

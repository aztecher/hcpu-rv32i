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
  (and#, or#, xor#, toInteger#, shiftL#, shiftR#, index#,
   msb#, replaceBit#, lt#, gt#, ge#, complement#, negate#, neq#)
import Clash.Sized.Vector (Vec((:>), Nil), (!!), (++), repeat )
import Clash.Sized.Signed (Signed)
import Clash.Sized.Internal.Signed (fromInteger#)
import Clash.Sized.Unsigned (Unsigned)
import Clash.Class.BitPack (pack, unpack)
import Clash.Class.Resize (resize)
import Clash.Promoted.Nat (snatProxy)
import Clash.Promoted.Nat.Literals as Nat
import Clash.XException (NFDataX)
import Data.Proxy
import GHC.Generics (Generic)
import Prelude
  (($), (+), (-), (*), (**), (==), (<), (.),
   undefined, take, fmap, otherwise, fromIntegral,
   Show, Eq, IO, Int, Bool(..), Float)
import GHC.Stack (HasCallStack)
import GHC.TypeLits

-- CPU State
data CPUActivity
  = LoadingInstruction
  | IncrementProgramCounter
  | ExecutingInstruction Instruction deriving (Show, Generic, NFDataX)

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
            registers' = writeRegister registers rd naddr
            pc' = Ptr $ unpack $
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
            ptr = Ptr $ readRegister registers rs1 + resize offset
            Word8 value = readRAM1Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LH  -> (CPUState IncrementProgramCounter registers', ram)
          where
            ptr = Ptr $ readRegister registers rs1 + resize offset
            Word16 value = readRAM2Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LW  -> (CPUState IncrementProgramCounter registers', ram)
          where
            ptr = Ptr $ readRegister registers rs1 + resize offset
            Word32 value = readRAM4Byte ram ptr
            result = resize value
            registers' = writeRegister registers rd result
        LBU -> (CPUState IncrementProgramCounter registers', ram)
          where
            zeroExpand :: Signed 8 -> Signed 32
            zeroExpand num = unpack $ (0 :: BitVector 24) ++# (pack num)
            ptr = Ptr $ readRegister registers rs1 + resize offset
            Word8 value = readRAM1Byte ram ptr
            result = zeroExpand value
            registers' = writeRegister registers rd result
        LHU -> (CPUState IncrementProgramCounter registers', ram)
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
        SB -> (CPUState IncrementProgramCounter registers, ram')
          where
            offset = unpack $ pack offset1 ++# pack offset2
            ptr    = Ptr $ readRegister registers rs1 + resize offset
            value  = Word8 $ resize $ readRegister registers rs2
            ram'   = writeRAM1Byte ram ptr value
        SH -> (CPUState IncrementProgramCounter registers, ram')
          where
            offset = unpack $ pack offset1 ++# pack offset2
            ptr    = Ptr $ readRegister registers rs1 + resize offset
            value  = Word16 $ resize $ readRegister registers rs2
            ram'   = writeRAM2Byte ram ptr value
        SW -> (CPUState IncrementProgramCounter registers, ram')
          where
            offset = unpack $ pack offset1 ++# pack offset2
            ptr    = Ptr $ readRegister registers rs1 + resize offset
            value  = Word32 $ readRegister registers rs2
            ram'   = writeRAM4Byte ram ptr value
    B format -> case format of
      BBranch br (Word7 offset1) rs2 rs1 (Word5 offset2) -> case br of
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
            result  = shifted + ad
            registers' = writeRegister registers rd result
    J format -> case format of
      JJal jal (Word20 offset) rd -> case jal of
        JAL -> (CPUState nActivity registers'', ram)
          where
            Ptr curr = pc registers
            Ptr naddr = increment (pc registers)
            registers' = writeRegister registers rd naddr
            pc' = Ptr $ (curr + (resize offset))
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
replaceFromMsr bv shift bit = case bit of
  (1 :: Bit) -> let mask = fetchBitMaskFromMsr shift bit
                in bv `or#` mask
  (0 :: Bit) -> let mask = fetchBitMaskFromMsr shift bit
                in bv `and#` mask

-- mkBit :: forall n . (KnownNat n, (n <=? 32) ~ 'True, (0 <=? n) ~ 'True)
--   => n
--   -> BitVector (32 - n)
-- mkBit from = slice from' to' source
--   where
--     from'  = snatProxy (Proxy :: Proxy 31)
--     to'    = snatProxy (Proxy :: Proxy from)
--     source = 0 :: BitVector 32

-- Hardcoded because the BitVector length is at mos 32.
fetchBitMaskFromMsr :: Int -> Bit -> BitVector 32
fetchBitMaskFromMsr num bit = case bit of
  (1 :: Bit) -> mask
  (0 :: Bit) -> xor# mask (-1 :: BitVector 32)
  where
    mask = case num of
      0 -> 0 :: BitVector 32
      1 -> 2147483648 :: BitVector 32
      2 -> 3221225472 :: BitVector 32
      3 -> 3758096384 :: BitVector 32
      4 -> 4026531840 :: BitVector 32
      5 -> 4160749568 :: BitVector 32
      6 -> 4227858432 :: BitVector 32
      7 -> 4261412864 :: BitVector 32
      8 -> 4278190080 :: BitVector 32
      9 -> 4286578688 :: BitVector 32
      10 -> 4290772992 :: BitVector 32
      11 -> 4292870144 :: BitVector 32
      12 -> 4293918720 :: BitVector 32
      13 -> 4294443008 :: BitVector 32
      14 -> 4294705152 :: BitVector 32
      15 -> 4294836224 :: BitVector 32
      16 -> 4294901760 :: BitVector 32
      17 -> 4294934528 :: BitVector 32
      18 -> 4294950912 :: BitVector 32
      19 -> 4294959104 :: BitVector 32
      20 -> 4294963200 :: BitVector 32
      21 -> 4294965248 :: BitVector 32
      22 -> 4294966272 :: BitVector 32
      23 -> 4294966784 :: BitVector 32
      24 -> 4294967040 :: BitVector 32
      25 -> 4294967168 :: BitVector 32
      26 -> 4294967232 :: BitVector 32
      27 -> 4294967264 :: BitVector 32
      28 -> 4294967280 :: BitVector 32
      29 -> 4294967288 :: BitVector 32
      30 -> 4294967292 :: BitVector 32
      31 -> 4294967294 :: BitVector 32
      32 -> 4294967295 :: BitVector 32
      _ -> undefined

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

-- You can play code in RV32I.RV32I.Programs.Example like follow
-- >>> Prelude.take 10 $ sample (cpu initCPUState (programmedRAM addImm) :: Signal System Registers)
cpu :: HiddenClockResetEnable dom => CPUState -> RAM -> Signal dom Registers
cpu = cpuHardware

-- runCPU' take 4 arguments s.t.
--   inst: CPU instruction
--   ptr : init program counter
--   n   : number of execution result
runCPU' inst ptr n = Prelude.take n $ sample (cpu (zeroRegisterCPU (Ptr ptr)) (programmedRAM inst) :: Signal System Registers)
runCPU inst ptr = runCPU' inst ptr 2000

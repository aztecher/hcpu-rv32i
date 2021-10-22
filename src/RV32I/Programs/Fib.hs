module RV32I.Programs.Fib where

import RV32I.DSL
import RV32I.Register
import Clash.Sized.Vector (Vec((:>), Nil))

-- The code to be converted（C)
-- int fib(int n) {
--   if (n <= 1) {
--     return 1;
--   }
--   return fib(n-1) + fib(n-2);
-- }
-- // calculate fibonacci(10) and after then infinite loop
-- int main(void) {
--   int result = fib(10);
--   for(;;) {}
--   return 0;
-- }

-- The reuslt of building above code for RV32I (Assembly)
-- 00010144 <fib>:
--    10144:       fe010113                addi    sp,sp,-32
--    10148:       00112e23                sw      ra,28(sp)
--    1014c:       00812c23                sw      s0,24(sp)
--    10150:       00912a23                sw      s1,20(sp)
--    10154:       02010413                addi    s0,sp,32
--    10158:       fea42623                sw      a0,-20(s0)
--    1015c:       fec42703                lw      a4,-20(s0)
--    10160:       00100793                li      a5,1
--    10164:       00e7c663                blt     a5,a4,10170 <fib+0x2c>
--    10168:       00100793                li      a5,1
--    1016c:       0300006f                j       1019c <fib+0x58>
--    10170:       fec42783                lw      a5,-20(s0)
--    10174:       fff78793                addi    a5,a5,-1
--    10178:       00078513                mv      a0,a5
--    1017c:       fc9ff0ef                jal     ra,10144 <fib>
--    10180:       00050493                mv      s1,a0
--    10184:       fec42783                lw      a5,-20(s0)
--    10188:       ffe78793                addi    a5,a5,-2
--    1018c:       00078513                mv      a0,a5
--    10190:       fb5ff0ef                jal     ra,10144 <fib>
--    10194:       00050793                mv      a5,a0
--    10198:       00f487b3                add     a5,s1,a5
--    1019c:       00078513                mv      a0,a5
--    101a0:       01c12083                lw      ra,28(sp)
--    101a4:       01812403                lw      s0,24(sp)
--    101a8:       01412483                lw      s1,20(sp)
--    101ac:       02010113                addi    sp,sp,32
--    101b0:       00008067                ret
-- 000101b4 <main>:
--    101b4:       fe010113                addi    sp,sp,-32
--    101b8:       00112e23                sw      ra,28(sp)
--    101bc:       00812c23                sw      s0,24(sp)
--    101c0:       02010413                addi    s0,sp,32
--    101c4:       00a00513                li      a0,10
--    101c8:       f7dff0ef                jal     ra,10144 <fib>
--    101cc:       fea42623                sw      a0,-20(s0)
--    101d0:       0000006f                j       101d0 <main+0x1c>

-- DSL codes that represents above assembly code
fibonacci n =
     addi SP SP (-32)  -- pc = 0
  :> sw   RA 28 SP     -- pc = 4
  :> sw   S0 24 SP     -- pc = 8
  :> sw   S1 20 SP     -- pc = 12
  :> addi S0 SP 32     -- pc = 16
  :> sw   A0 (-20) S0  -- pc = 20
  :> lw   A4 (-20) S0  -- pc = 24
  :> li   A5 1         -- pc = 28
  :> blt  A5 A4 12     -- pc = 32 / if True then jamp to pc=44
  :> li   A5 1         -- pc = 36
  :> j    48           -- pc = 40 / jamp to pc=88
  :> lw   A5 (-20) S0  -- pc = 44
  :> addi A5 A5 (-1)   -- pc = 48
  :> mv   A0 A5        -- pc = 52
  :> jal  RA (-56)     -- pc = 56 / return to fib first
  :> mv   S1 A0        -- pc = 60
  :> lw   A5 (-20) S0  -- pc = 64
  :> addi A5 A5 (-2)   -- pc = 68
  :> mv   A0 A5        -- pc = 72
  :> jal  RA (-76)     -- pc = 76 / return to fib first
  :> mv   A5 A0        -- pc = 80
  :> add  A5 S1 A5     -- pc = 84
  :> mv   A0 A5        -- pc = 88
  :> lw   RA 28 SP     -- pc = 92
  :> lw   S0 24 SP     -- pc = 96
  :> lw   S1 20 SP     -- pc = 100
  :> addi SP SP 32     -- pc = 104
  :> ret               -- pc = 108
  -- start main function
  :> addi SP SP 2016   -- pc = 112
  :> addi SP SP (-32)  -- pc = 116
  :> sw   RA 28  SP    -- pc = 120
  :> sw   S0 24  SP    -- pc = 124
  :> addi S0 SP 32     -- pc = 128
  :> li   A0 n         -- pc = 132 / n of fib(n=10)
  :> jal  RA (-136)    -- pc = 136 / jamp to Fibonacci
  :> j 0               -- pc = 140 / jamp here and loop
  :> Nil
  where
    -- Pseudo instructions
    mv rd rs1 = addi rd rs1 0
    li rd imm = addi rd T6 imm
    j  offset = jal Zero offset
    ret       = jalr Zero 0 RA

-- Example) fib(10) = 89 (this example takes too many time, but calculation success!)
-- λ > :m + Text.Pretty.Simple
-- λ > runCPU inst = Prelude.take 20000 $ sample (cpu (zeroRegisterCPU (Ptr  112)) (programmedRAM inst) :: Signal System Registers)
-- λ > pPrint $ Prelude.last $ runCPU (fibonacci 10)
-- Registers
--     { zero = 144
--     , ra = 140
--     , sp = 1984
--     , gp = 0
--     , tp = 0
--     , t0 = 0
--     , t1 = 0
--     , t2 = 0
--     , s0fp = 2016
--     , s1 = 0
--     , a0 = 89
--     , a1 = 0
--     , a2 = 0
--     , a3 = 0
--     , a4 = 0
--     , a5 = 89
--     , a6 = 0
--     , a7 = 0
--     , s2 = 0
--     , s3 = 0
--     , s4 = 0
--     , s5 = 0
--     , s6 = 0
--     , s7 = 0
--     , s8 = 0
--     , s9 = 0
--     , s10 = 0
--     , s11 = 0
--     , t3 = 0
--     , t4 = 0
--     , t5 = 0
--     , t6 = 0
--     , pc = Ptr 140
--     }

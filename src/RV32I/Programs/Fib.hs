module RV32I.Programs.Fib where

import RV32I.DSL
import RV32I.Register
import Clash.Sized.Vector (Vec((:>), Nil))

-- 変換対象のコード（C)
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

-- rv32i 向けに build した結果 (assembly)
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

fibonacci =
     addi SP SP (-32)  -- pc = 0
  :> sw   RA 28 SP     -- pc = 32
  :> sw   S0 24 SP     -- pc = 64
  :> sw   S1 20 SP     -- pc = 96
  :> addi S0 SP 32     -- pc = 128
  :> sw   A0 (-20) S0  -- pc = 160
  :> lw   A4 (-20) S0  -- pc = 192
  :> li   A5 1         -- pc = 224
  :> blt  A5 A4 96     -- pc = 256 / if True then jamp to pc=352
  :> li   A5 1         -- pc = 288
  :> j    384          -- pc = 320 / jamp to pc=704
  :> lw   A5 (-20) S0  -- pc = 352
  :> addi A5 A5 (-1)   -- pc = 384
  :> mv   A0 A5        -- pc = 416
  :> jal  RA (-448)    -- pc = 448 / return to fib first
  :> mv   S1 A0        -- pc = 480
  :> lw   A5 (-20) S0  -- pc = 512
  :> addi A5 A5 (-2)   -- pc = 554
  :> mv   A0 A5        -- pc = 576
  :> jal  RA (-608)    -- pc = 608 / return to fib first
  :> mv   A5 A0        -- pc = 640
  :> add  A5 S1 A5     -- pc = 672
  :> mv   A0 A5        -- pc = 704
  :> lw   RA 28 SP     -- pc = 736
  :> lw   S0 24 SP     -- pc = 768
  :> lw   S1 20 SP     -- pc = 800
  :> addi SP SP 32     -- pc = 832
  :> ret               -- pc = 864
  -- start main function
  :> addi SP SP 2016  -- pc = 896 / program data are located in 'Highest address - offset'
  :> addi SP SP (-32 * 4)  -- pc = 928
  :> sw   RA 28  SP    -- pc = 960
  :> sw   S0 24  SP    -- pc = 992
  :> addi S0 SP 32     -- pc = 1024
  :> li   A0 3         -- pc = 1056 / fib(n=3) of n
  :> jal  RA (-1088)   -- pc = 1088 / jamp to Fibonacci
  :> j 0               -- pc = 1120 / jamp here and loop
  :> Nil
  where
    -- Pseudo instructions
    mv rd rs1 = addi rd rs1 0
    li rd imm = addi rd T6 imm
    j  offset = jal Zero offset
    ret       = jalr Zero 0 RA


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TestUtils.Common where

import RV32I.Word
import RV32I.Format
import RV32I.Register
import RV32I.Instruction
import RV32I.RAM
import RV32I.CPU
import Clash.Sized.Vector (Vec (Nil, (:>)), listToVecTH, repeat)
import Clash.Sized.BitVector (BitVector)
import Clash.Sized.Signed (Signed)
import Prelude hiding (repeat)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck as QC


obviously :: [Int] -> Bool
obviously _ = True

-- helper functions for unit test
propTag :: String
propTag = "[PROP] "

unitTag :: String
unitTag = "[Unit] "


-- helper functions for property-based-testing
zeroRAM :: RAM
zeroRAM = (RAM (repeat (Word8 0)))

zeroRegisters :: Registers
zeroRegisters = (Registers 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (Ptr 0))


instance Arbitrary Register where
  arbitrary = QC.chooseEnum (Zero, T6)

instance Arbitrary Word32 where
  arbitrary = do
    gen <- QC.choose (-2147483648 :: Integer, 2147483647)
    return $ Word32 (fromIntegral gen :: Signed 32)

instance Arbitrary Word20 where
  arbitrary = do
    gen <- QC.choose (-524288 :: Integer, 524287)
    return $ Word20 (fromIntegral gen :: Signed 20)

instance Arbitrary Word16 where
  arbitrary = do
    gen <- QC.choose (-32768 :: Integer, 32767)
    return $ Word16 (fromIntegral gen :: Signed 16)

instance Arbitrary Word12 where
  arbitrary = do
    gen <- QC.choose (-2048 :: Integer, 2047)
    return $ Word12 (fromIntegral gen :: Signed 12)

instance Arbitrary Word8 where
  arbitrary = do
    gen <- QC.choose (-128 :: Integer, 127)
    return $ Word8 (fromIntegral gen :: Signed 8)

instance Arbitrary Word7 where
  arbitrary = do
    gen <- QC.choose (-64 :: Integer, 63)
    return $ Word7 (fromIntegral gen :: Signed 7)

instance Arbitrary Word5 where
  arbitrary = do
    gen <- QC.choose (-16 :: Integer, 15)
    return $ Word5 (fromIntegral gen :: Signed 5)

instance Arbitrary Word4 where
  arbitrary = do
    gen <- QC.choose (-8 :: Integer, 7)
    return $ Word4 (fromIntegral gen :: Signed 4)

instance Arbitrary RArith where
  arbitrary = QC.chooseEnum (ADD, SLTU)

instance Arbitrary IArith where
  arbitrary = QC.chooseEnum (ADDI, ANDI)

instance Arbitrary IShift where
  arbitrary = QC.chooseEnum (SLLI, SRLI)

instance Arbitrary IJalr where
  arbitrary = QC.chooseEnum (JALR, JALR)

instance Arbitrary ILoad where
  arbitrary = QC.chooseEnum (LB, LHU)

instance Arbitrary IFence where
  arbitrary = QC.chooseEnum (FENCE, FENCE)

instance Arbitrary IFencei where
  arbitrary = QC.chooseEnum (FENCEI, FENCEI)

instance Arbitrary IEnv where
  arbitrary = QC.chooseEnum (EBREAK, ECALL)

instance Arbitrary ICsr where
  arbitrary = QC.chooseEnum (CSRRC, CSRRW)

instance Arbitrary ICsri where
  arbitrary = QC.chooseEnum (CSRRCI, CSRRWI)

instance Arbitrary SStore where
  arbitrary = QC.chooseEnum (SB, SW)

instance Arbitrary BBranch where
  arbitrary = QC.chooseEnum (BEQ, BGEU)

instance Arbitrary UArith where
  arbitrary = QC.chooseEnum (LUI, AUIPC)

instance Arbitrary JJal where
  arbitrary = QC.chooseEnum (JAL, JAL)

instance Arbitrary RFormat where
  arbitrary = do
    arith <- arbitrary :: Gen RArith
    reg1 <- arbitrary :: Gen Register
    reg2 <- arbitrary :: Gen Register
    reg3 <- arbitrary :: Gen Register
    return $ RArith arith reg1 reg2 reg3

instance Arbitrary IFormat where
  arbitrary = do
    iarith <- arbitrary :: Gen IArith
    ishift <- arbitrary :: Gen IShift
    ijalr <- arbitrary :: Gen IJalr
    iload <- arbitrary :: Gen ILoad
    ifence <- arbitrary :: Gen IFence
    ifencei <- arbitrary :: Gen IFencei
    ienv <- arbitrary :: Gen IEnv
    icsr <- arbitrary :: Gen ICsr
    icsri <- arbitrary :: Gen ICsri
    word12 <- arbitrary :: Gen Word12
    word5 <- arbitrary :: Gen Word5
    word4 <- arbitrary :: Gen Word4
    reg <- arbitrary :: Gen Register
    elements
      [ (IArith iarith word12 reg reg),
        (IShift ishift word5 reg reg),
        (IJalr ijalr word12 reg reg),
        (ILoad iload word12 reg reg),
        (IFence ifence word4 word4),
        (IFencei ifencei),
        (IEnv ienv),
        (ICsr icsr word12 reg reg),
        (ICsri icsri word12 word5 reg)
      ]

instance Arbitrary SFormat where
  arbitrary = do
    sstore <- arbitrary :: Gen SStore
    word7 <- arbitrary :: Gen Word7
    reg1 <- arbitrary :: Gen Register
    reg2 <- arbitrary :: Gen Register
    word5 <- arbitrary :: Gen Word5
    return $ SStore sstore word7 reg1 reg2 word5

instance Arbitrary BFormat where
  arbitrary = do
    bbranch <- arbitrary :: Gen BBranch
    word7 <- arbitrary :: Gen Word7
    reg1 <- arbitrary :: Gen Register
    reg2 <- arbitrary :: Gen Register
    word5 <- arbitrary :: Gen Word5
    return $ BBranch bbranch word7 reg1 reg2 word5

instance Arbitrary UFormat where
  arbitrary = do
    uarith <- arbitrary :: Gen UArith
    word20 <- arbitrary :: Gen Word20
    reg <- arbitrary :: Gen Register
    return $ UArith uarith word20 reg

instance Arbitrary JFormat where
  arbitrary = do
    jjal <- arbitrary :: Gen JJal
    word20 <- arbitrary :: Gen Word20
    reg <- arbitrary :: Gen Register
    return $ JJal jjal word20 reg

instance Arbitrary Instruction where
  arbitrary = do
    rformat <- arbitrary :: Gen RFormat
    iformat <- arbitrary :: Gen IFormat
    sformat <- arbitrary :: Gen SFormat
    bformat <- arbitrary :: Gen BFormat
    uformat <- arbitrary :: Gen UFormat
    jformat <- arbitrary :: Gen JFormat
    elements
      [ (R rformat),
        (I iformat),
        (S sformat),
        (B bformat),
        (U uformat),
        (J jformat)
      ]

instance Arbitrary RAM where
  arbitrary = do
    vec <- arbitrary :: Gen (Vec 2048 Word8)
    return $ RAM vec

instance Arbitrary Ptr where
  arbitrary = do
    gen <- QC.choose (-2147483648 :: Integer, 2147483647)
    return $ Ptr (fromIntegral gen :: Signed 32)

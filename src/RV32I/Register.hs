module RV32I.Register where

import Clash.Sized.BitVector (BitVector)
import Clash.XException (NFDataX)
import GHC.Generics (Generic)
import Prelude (Show, Eq, Enum)

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


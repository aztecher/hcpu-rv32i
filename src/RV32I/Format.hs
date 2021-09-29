module RV32I.Format where

import RV32I.Register
import RV32I.Word
import Clash.XException (NFDataX)
import GHC.Generics (Generic)

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
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- I Opcode
data IArith
  = ADDI
  | SLTI
  | SLTIU
  | XORI
  | ORI
  | ANDI
  deriving (Show, Generic, NFDataX, Eq, Enum)

data IShift
  = SLLI
  | SRAI
  | SRLI
  deriving (Show, Generic, NFDataX, Eq, Enum)

data IJalr = JALR deriving (Show, Generic, NFDataX, Eq, Enum)

data ILoad
  = LB
  | LH
  | LW
  | LBU
  | LHU
  deriving (Show, Generic, NFDataX, Eq, Enum)

data IFence = FENCE deriving (Show, Generic, NFDataX, Eq, Enum)

data IFencei = FENCEI deriving (Show, Generic, NFDataX, Eq, Enum)

data IEnv
  = EBREAK
  | ECALL
  deriving (Show, Generic, NFDataX, Eq, Enum)

data ICsr
  = CSRRC
  | CSRRS
  | CSRRW
  deriving (Show, Generic, NFDataX, Eq, Enum)

data ICsri
  = CSRRCI
  | CSRRSI
  | CSRRWI
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- S Opcode
data SStore
  = SB
  | SH
  | SW
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- B Opcode
data BBranch
  = BEQ
  | BNE
  | BLT
  | BGE
  | BLTU
  | BGEU
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- U Opcode
data UArith
  = LUI
  | AUIPC
  deriving (Show, Generic, NFDataX, Eq, Enum)

-- J Opcode
data JJal = JAL deriving (Show, Generic, NFDataX, Eq, Enum)


-- R Format
data RFormat
  = RArith RArith
           Register
           Register
           Register
  deriving (Show, Generic, NFDataX, Eq)

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
  deriving (Show, Generic, NFDataX, Eq)

-- S Format
data SFormat
  = SStore SStore
           Word7
           Register
           Register
           Word5
  deriving (Show, Generic, NFDataX, Eq)

-- B Format
data BFormat
  = BBranch BBranch
            Word7
            Register
            Register
            Word5
  deriving (Show, Generic, NFDataX, Eq)

-- U Format
data UFormat
  = UArith UArith
           Word20
           Register
  deriving (Show, Generic, NFDataX, Eq)

-- J Format
data JFormat
  = JJal JJal
         Word20
         Register
  deriving (Show, Generic, NFDataX, Eq)

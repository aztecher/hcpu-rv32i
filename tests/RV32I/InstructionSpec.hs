module RV32I.InstructionSpec where

import RV32I.Instruction
import TestUtils.Common
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC
import Prelude (map, (<>), ($), (==), (.), Bool(..))


-- encode/decode from/to Instruction/Word32 will be satisfied identification
decode_encode_instruction :: [Instruction] -> Bool
decode_encode_instruction xs = map (decodeInstruction . encodeInstruction) xs == xs

spec :: Spec
spec = do
  describe (propTag <> "decodeInstruction . encodeInstruction") $ do
    prop "check decode / encode Instruction identity property" decode_encode_instruction

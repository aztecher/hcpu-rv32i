module RV32I.RegisterSpec where

import RV32I.Register
import TestUtils.Common
import Clash.Sized.BitVector (BitVector)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC


-- encode/decode from/to Register/BitVector5 will be satisfied identification.
decode_encode_register :: [Register] -> Bool
decode_encode_register xs = map (decodeRegister . encodeRegister) xs == xs

encode_decode_bitvector :: [BitVector 5] -> Bool
encode_decode_bitvector xs = map (encodeRegister . decodeRegister) xs == xs

spec = do
  describe (propTag <> "decodeRegister . encodeRegister") $ do
    prop "check decode / encode Register identity property" $ decode_encode_register
  describe (propTag <> "encodeRegister . decodeRegister") $ do
    prop "check encode / decode BitVector5 identity property" $ encode_decode_bitvector

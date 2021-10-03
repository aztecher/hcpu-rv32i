module RV32I.WordSpec where

import RV32I.Word

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC
import TestUtils.Common

concat_separate_word8_word32 :: [Word32] -> Bool
concat_separate_word8_word32 xs
  = map ((seqApply concatWord8ToWord32) . separateWord32ToWord8) xs == xs
  where
    seqApply :: (Word8 -> Word8 -> Word8 -> Word8 -> Word32)
      -> (Word8, Word8, Word8, Word8)
      -> Word32
    seqApply f (n1, n2, n3, n4) = f n1 n2 n3 n4

separate_concat_word32_word8 :: [(Word8, Word8, Word8, Word8)] -> Bool
separate_concat_word32_word8 xs
  = map (separateWord32ToWord8 . (seqApply concatWord8ToWord32)) xs == xs
  where
    seqApply :: (Word8 -> Word8 -> Word8 -> Word8 -> Word32)
      -> (Word8, Word8, Word8, Word8)
      -> Word32
    seqApply f (n1, n2, n3, n4) = f n1 n2 n3 n4

concat_separate_word8_word16 :: [Word16] -> Bool
concat_separate_word8_word16 xs
  = map ((seqApply concatWord8ToWord16) . separateWord16ToWord8) xs == xs
  where
    seqApply :: (Word8 -> Word8 -> Word16)
      -> (Word8, Word8)
      -> Word16
    seqApply f (n1, n2) = f n1 n2

separate_concat_word16_word8 :: [(Word8, Word8)] -> Bool
separate_concat_word16_word8 xs
  = map (separateWord16ToWord8 . (seqApply concatWord8ToWord16)) xs == xs
  where
    seqApply :: (Word8 -> Word8 -> Word16)
      -> (Word8, Word8)
      -> Word16
    seqApply f (n1, n2) = f n1 n2

spec :: Spec
spec = do
  describe (propTag <> "concat and separate between Word8 and Word32") $ do
    prop "check concatWord8ToWord32 / separateWord32ToWord8" $ concat_separate_word8_word32
  describe (propTag <> "concat and separate between Word8 and Word16") $ do
    prop "check concatWord8ToWord16 / separateWord16ToWord8" $ concat_separate_word8_word16
  describe (propTag <> "separate and concat between Word8 and word32") $ do
    prop "check separateWord32ToWord8 / concatWord8ToWord32" $ separate_concat_word32_word8
  describe (propTag <> "separate and concat between Word8 and Word32") $ do
    prop "check separateWord16ToWord8 / concatWord8ToWord16" $ separate_concat_word16_word8

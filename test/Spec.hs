{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Data.ByteString.Builder
import Data.RSV
import Data.Text
import Data.Vector as V
import Test.Hspec

data Person = Person {
  name :: !Text,
  age  :: !(Maybe Int)
}

instance Row Person where
  values Person{..} = [encodeUtf8 name, encodeUtf8 age]

main :: IO ()
main = hspec $ do 
  describe "encode" $ do
    it "properly encodes a list of lists of Ints" $ do
      let expected = toLazyByteString $
            stringUtf8 (show (3 :: Int)) <> valueTerminator <>
            stringUtf8 (show (4 :: Int)) <> valueTerminator <>
            stringUtf8 (show (5 :: Int)) <> valueTerminator <>
            rowTerminator
      let numbers = [[3, 4, 5]] :: [[Int]]
      let encoded = encode numbers 
      encoded `shouldBe` expected
    it "properly encodes a Foldable of any type which implements Row" $ do
      let expected = toLazyByteString $
            stringUtf8 "Morrissey" <> valueTerminator <>
            nullValue <> valueTerminator <>
            rowTerminator
      let person = Person "Morrissey" Nothing
      let encoded = encode $ V.singleton person 
      encoded `shouldBe` expected

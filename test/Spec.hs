{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Data.RSV
import Data.Text
import Test.Hspec

import qualified Data.ByteString.Lazy as LB

data Person = Person { firstName :: !Text, lastName :: !Text, age :: !(Maybe Int) } deriving (Eq, Show)

instance FromRow Person where
  fromRow = parseRow $ Person <$> fromValue <*> fromValue <*> fromValue

main :: IO ()
main = hspec $ do 
  describe "parse" $ do
    it "parses" $ do
      let lbs = LB.pack [0x79, valueTerminatorChar, 0x66, valueTerminatorChar, rowTerminatorChar, 0x79, valueTerminatorChar, 0x66, valueTerminatorChar, rowTerminatorChar]
      let result :: Either IndexedException [(Text, Text)] = parse lbs
      case result of
        Left e -> expectationFailure (show e)
        Right a -> a `shouldBe` [("y", "f"), ("y", "f")] 
    it "parses Maybe" $ do
      let lbs = LB.pack [0x79, valueTerminatorChar, 0x66, valueTerminatorChar, rowTerminatorChar, nullChar, valueTerminatorChar, 0x66, valueTerminatorChar, rowTerminatorChar]
      let result :: Either IndexedException [(Maybe Text, Maybe Text)] = parse lbs
      case result of
        Left e -> expectationFailure (show e)
        Right a -> a `shouldBe` [(Just "y", Just "f"), (Nothing, Just "f")] 
    it "throws a Unicode error at the correct indices" $ do
      let invalidUtf8 = LB.pack [0x79, valueTerminatorChar, 0x80, 0x81, 0xC0, 0xC1, 0x90, valueTerminatorChar, rowTerminatorChar]
      let result :: Either IndexedException [[Text]] = parse invalidUtf8 
      case result of
        -- The indices mean:
        -- 1. The byte index is 8. We can ignore this one for a unicode error.
        -- 2. The value index is 2. This is the byte offset of the offending value.
        -- 3. The row index is 0. This is the byte offset of the offending row.
        Left e -> e `shouldBe` (ParserIndices 8 2 0, UnicodeError)
        Right _ -> expectationFailure "What?"
    it "throws a ConversionError at the correct indices" $ do
      let invalidInt = LB.pack [0x79, 0x66, valueTerminatorChar, rowTerminatorChar]
      let result :: Either IndexedException [[Int]] = parse invalidInt
      case result of
        Left e -> e `shouldBe` (ParserIndices 3 0 0, ConversionError "Could not convert string yf to desired type")
        Right _ -> expectationFailure "Hunh?"

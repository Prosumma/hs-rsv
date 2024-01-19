{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad.IO.Class
import Data.Default
import Data.RSV
import Data.Text
import Test.Hspec

import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as Set

data Person = Person { firstName :: !Text, lastName :: !Text, year :: !(Maybe Int) } deriving (Eq, Show)

instance ToRow Person where
  toRow Person{..} = encodeRow $ toValue firstName <+> toValue lastName <+> toValue year

instance FromRow Person where
  fromRow = parseRow $ Person <$> fromValue <*> fromValue <*> fromValue

data X = XInt !Int | XText !Text deriving (Eq, Show)

instance ToValue X where
  toValue (XInt i) = toValue i
  toValue (XText t) = toValue t

instance FromValue X where
  fromValue = (XInt <$> fromValue) <|> XText <$> fromValue

roundtripWith :: (Foldable t, ToRow a, FromRow a) => ParserConfig -> t a -> Either IndexedException [a]
roundtripWith config = parseWith config . encodeWith config

roundtrip :: (Foldable t, ToRow a, FromRow a) => t a -> Either IndexedException [a]
roundtrip = roundtripWith def 

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
        -- 1. The byte index is 8. We can ignore this one for a unicode error because it applies to the whole value.
        -- 2. The value index is 2. This is the byte offset of the offending value.
        -- 3. The row index is 0. This is the byte offset of the offending row.
        Left e -> e `shouldBe` (ParserIndices 8 2 0, UnicodeError)
        Right _ -> expectationFailure "What?"
    it "throws a ConversionError at the correct indices" $ do
      let invalidInt = LB.pack [0x79, 0x66, valueTerminatorChar, rowTerminatorChar]
      let result :: Either IndexedException [[Int]] = parse invalidInt
      case result of
        Left e -> e `shouldBe` (ParserIndices 3 0 0, ConversionError "Could not convert string yf to desired type.")
        Right _ -> expectationFailure "Hunh?"
    it "x's" $ do
      let xs = [[XInt 3, XText "foo"]]
      liftIO $ print $ encode xs
      case roundtrip xs of
        Left e -> expectationFailure (show e)
        Right xs' -> xs' `shouldBe` xs
  describe "encode then parse" $ do
    it "is a roundtrip ticket" $ do
      let people :: [Person] = [
              Person "Dave" "Gahan" (Just 1962),
              Person "Stephen" "Morrissey" (Just 1959)
            ] 
      liftIO $ print $ encode people
      case roundtrip people of
        Left e -> expectationFailure (show e)
        Right people' -> people' `shouldBe` people
  describe "bool" $ do
    it "is configurable" $ do
      let currentFalseValues = falseValues (def :: ParserConfig)
      let config = def {falseValues=currentFalseValues<>Set.fromList ["gronk"],falseValue="gronk"}
      let bools = [[True, False, True]]
      case roundtripWith config bools of
        Left e -> expectationFailure (show e)
        Right bools' -> bools' `shouldBe` bools
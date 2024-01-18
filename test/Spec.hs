{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

import Control.Applicative
import Data.ByteString.Lazy as LB hiding (drop)
import Data.RSV
import Data.Text as T hiding (drop)
import Test.Hspec

data Person = Person {
  name :: !Text,
  num  :: !(Maybe Int)
} deriving (Eq, Show) 

data PersonOrName = PersonOnly !Person | NameOnly !Text deriving (Eq, Show)

instance ToRSVRow Person where
  toRSVRow Person{..} = encodeRow $ toRSV name <> toRSV num

instance FromRSVRow Person where
  fromRSVRow = parseRow $ Person <$> fromRSV <*> fromRSV

instance ToRSVRow PersonOrName where
  toRSVRow (PersonOnly person) = toRSVRow person
  toRSVRow (NameOnly name) = toRSVRow [name]

instance FromRSVRow PersonOrName where
  fromRSVRow = (PersonOnly <$> fromRSVRow) <|> parseRow (NameOnly <$> fromRSV)

main :: IO ()
main = hspec $ do
  describe "parseValue" $ do
    it "throws UnpermittedNull on null" $ do
      let bytes = [nullChar, valueTerminatorChar]
      case evalRSVParser (0, bytes) parseValue of
        Left (p, UnpermittedNull) -> do
          p `shouldBe` 0
        Left e -> expectationFailure $ "Expected unpermitted null, but got " <> show e
        Right _ -> expectationFailure "Expected evaluation to fail, but it suceeded."
  describe "permitNull" $ do
    it "permits a null" $ do
      let bytes = [nullChar, valueTerminatorChar]
      case evalRSVParser (0, bytes) (permitNull parseValue) of
        Left e -> expectationFailure (show e)
        Right p -> p `shouldBe` Nothing
    it "advances the state properly" $ do
      let bytes = [nullChar, valueTerminatorChar]
      case execRSVParser (0, bytes) (permitNull parseValue) of
        Left e -> expectationFailure (show e)
        Right s -> s `shouldBe` (2, [])
  describe "parseRead" $ do
    it "reads an Int and properly advances the state" $ do
      let bytes = [0x31, 0x32, valueTerminatorChar]
      case runRSVParser (0, bytes) parseRead of
        Left e -> expectationFailure (show e)
        Right (n, s) -> do
          n `shouldBe` (12 :: Int)
          s `shouldBe` (3, [])
    it "properly rewinds the state in the case of InvalidFormat" $ do
      let bytes = [0x31, 0x79, valueTerminatorChar]
      case runRSVParser (0, bytes) (parseRead :: RSVParser Int) of
        Left (p, InvalidFormat) -> p `shouldBe` 0
        _ -> expectationFailure "Expected to fail, but it succeeded."
  describe "parseList" $ do
    it "parses a list" $ do
      let input :: [[Text]] = [["encode", "this"], ["then", "decode"]]
      let encoded = encode input
      let bytes = LB.unpack encoded
      let dl = parseList :: RSVParser [Text]
      case runRSVParser (0, bytes) dl of
        Left e -> expectationFailure (show e)
        Right (a, (p, _)) -> do
          a `shouldBe` ["encode", "this"]
          p `shouldBe` 13
          bytes !! 13 `shouldBe` 0x74
  describe "encode and parse" $ do
    it "roundtrips [[Maybe Text]]" $ do
      let input :: [[Maybe Text]] = [[Just "encode", Just "this"], [Just "then", Just "decode"]]
      let encoded = encode input
      case parse encoded of
        Left e -> expectationFailure (show e)
        Right a -> a `shouldBe` input
    it "roundtrips [Person]" $ do
      let input = [Person "Greg" (Just 3), Person "Rose" (Just 9)]
      let encoded = encode input
      case parse encoded of
        Left e -> expectationFailure (show e)
        Right a -> a `shouldBe` input
    it "roundtrips different types" $ do
      let input = [Person "Greg" (Just 3), Person "Rose" (Just 9)]
      let encoded = encode input
      let expected :: [[Text]] = [["Greg", "3"], ["Rose", "9"]]
      case parse encoded of
        Left e -> expectationFailure (show e)
        Right a -> a `shouldBe` expected 
    it "supports jagged rows" $ do
      let input = [NameOnly "Ludwig", PersonOnly (Person "Karl" Nothing)]
      let encoded = encode input
      case parse encoded of
        Left e -> expectationFailure (show e)
        Right a -> a `shouldBe` input 

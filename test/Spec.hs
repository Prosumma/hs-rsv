{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

import Control.Monad.State
import Data.ByteString.Lazy as LB hiding (drop)
import Data.RSV
import Data.Text as T hiding (drop)
import Test.Hspec
import Text.Read

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
  fromRSVRow = do
    list <- parseList
    case list of
      [Just name] -> return $ NameOnly name
      [Just name, Just num] -> case readMaybe (T.unpack num) of
        Nothing -> throwRSVException InvalidFormat 
        num -> return $ PersonOnly $ Person name num
      [Just name, Nothing] -> return $ PersonOnly $ Person name Nothing
      _ -> throwRSVException InvalidFormat

main :: IO ()
main = hspec $ do
  describe "parseByteString" $ do
    it "does cool things" $ do
      let bytes = [0x79, valueTerminatorChar, rowTerminatorChar]
      case parseByteString (0, bytes) of
        Left _ -> expectationFailure "This should have succeeded"
        Right ((p, _), b) -> do
          p `shouldBe` 2
          b `shouldBe` Just "y"
    it "processes nulls properly" $ do
      let bytes = [nullChar, valueTerminatorChar, 0x79, valueTerminatorChar]
      case parseByteString (0, bytes) of
        Left _ -> expectationFailure "This should have succeeded"
        Right ((p, ws), b) -> do
          p `shouldBe` 2
          ws `shouldBe` drop 2 bytes
          b `shouldBe` Nothing
    it "fails when null is misused" $ do
      let bytes = [nullChar, 0x79, valueTerminatorChar, 0x79, valueTerminatorChar]
      case parseByteString (0, bytes) of
        Left (p, UnexpectedNull) -> p `shouldBe` 0
        Left e -> expectationFailure (show e)
        _ -> expectationFailure "should have failed with UnpermittedNull"
  describe "parseValue" $ do
    it "throws UnpermittedNull on null" $ do
      let bytes = [nullChar, valueTerminatorChar]
      case evalStateT parseValue (0, bytes) of
        Left (p, UnpermittedNull) -> do
          p `shouldBe` 0
        Left e -> expectationFailure $ "Expected unpermitted null, but got " <> show e
        Right _ -> expectationFailure "Expected evaluation to fail, but it suceeded."
  describe "permitNull" $ do
    it "permits a null" $ do
      let bytes = [nullChar, valueTerminatorChar]
      case evalStateT (permitNull parseValue) (0, bytes) of
        Left e -> expectationFailure (show e)
        Right p -> p `shouldBe` Nothing
    it "advances the state properly" $ do
      let bytes = [nullChar, valueTerminatorChar]
      case execStateT (permitNull parseValue) (0, bytes) of
        Left e -> expectationFailure (show e)
        Right s -> s `shouldBe` (2, [])
  describe "parseRead" $ do
    it "reads an Int and properly advances the state" $ do
      let bytes = [0x31, 0x32, valueTerminatorChar]
      case runStateT parseRead (0, bytes) of
        Left e -> expectationFailure (show e)
        Right (n, s) -> do
          n `shouldBe` (12 :: Int)
          s `shouldBe` (3, [])
    it "properly rewinds the state in the case of InvalidFormat" $ do
      let bytes = [0x31, 0x79, valueTerminatorChar]
      case runStateT (parseRead :: RSVParser Int) (0, bytes) of
        Left (p, InvalidFormat) -> p `shouldBe` 0
        _ -> expectationFailure "Expected to fail, but it succeeded."
  describe "parseList" $ do
    it "parses a list" $ do
      let input :: [[Text]] = [["encode", "this"], ["then", "decode"]]
      let encoded = encode input
      let bytes = LB.unpack encoded
      let dl = parseList :: RSVParser [Text]
      case runStateT dl (0, bytes) of
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

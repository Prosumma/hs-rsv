{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

import Data.ByteString.Lazy as LB
import Data.RSV
import Data.Text
import Test.Hspec

data Person = Person {
  name :: !Text,
  age  :: !(Maybe Int)
} deriving (Eq, Show)

instance ToRSVRow Person where
  toRSVRow Person{..} = encodeRow $ toRSV name <> toRSV age

instance FromRSVRow Person where
  fromRSVRow = decodeRow $ Person <$> fromRSV <*> fromRSV

main :: IO ()
main = hspec $ do 
  describe "parseBytes" $ do
    it "works" $ do
      let b = LB.pack [0x71, valueTerminatorChar, nullChar, valueTerminatorChar, 0x79, valueTerminatorChar, rowTerminatorChar]
      let e = decode b 
      let expected :: [[Maybe Text]] = [[Just "q", Nothing, Just "y"]]
      case e of
        Left e -> expectationFailure (show e) 
        Right a -> a `shouldBe` expected
  describe "encode" $ do
    it "encodes" $ do
      let persons = [Person "Rose" Nothing, Person "Greg" (Just 2)]
      let e = encode persons
      case decode e of
        Left e -> expectationFailure (show e) 
        Right a -> a `shouldBe` persons

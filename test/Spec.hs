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

infix 1 `shouldBeRSV`

shouldBeRSV :: (Eq a, Show a) => Either DecodeException a -> a -> Expectation
shouldBeRSV a b = case a of
  Left e -> expectationFailure (show e)
  Right a -> a `shouldBe` b 

main :: IO ()
main = hspec $ do 
  describe "decode" $ do
    it "decodes" $ do
      let b = LB.pack [0x71, valueTerminatorChar, nullChar, valueTerminatorChar, 0x79, valueTerminatorChar, rowTerminatorChar]
      let e = decode b 
      let expected :: [[Maybe Text]] = [[Just "q", Nothing, Just "y"]]
      e `shouldBeRSV` expected
  describe "encode" $ do
    it "encodes" $ do
      let persons = [Person "Rose" Nothing, Person "Greg" (Just 2)]
      let e = encode persons
      decode e `shouldBeRSV` persons
    it "properly encodes any Foldable as a row when the elements are ToRSV" $ do
      let texts :: [[Maybe Text]] = [[Just "Mises", Just "Rothbard"]] 
      let e = encode texts
      decode e `shouldBeRSV` texts

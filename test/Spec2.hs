{-# LANGUAGE OverloadedStrings #-}

module Spec2 where

import Control.Monad.State
import Data.RSV2
import Test.Hspec

rsv2 :: Spec
rsv2 = do
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

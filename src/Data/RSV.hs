{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Data.RSV (
  assertRowTerminator,
  atRowTerminator,
  decode,
  decodeList,
  decodeList',
  decodeRow,
  decodeString,
  decodeText,
  encode,
  encodeNull,
  encodeShow,
  encodeStringUnsafe,
  encodeRow,
  encodeText,
  nullChar,
  rowTerminatorChar,
  valueTerminatorChar,
  DecodeException(..),
  FromRSV(..),
  FromRSVRow(..),
  RSVBuilder,
  RSVParser,
  ToRSV(..),
  ToRSVRow(..)
) where

import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.State as State
import Data.Bifunctor
import Data.ByteString.Builder
import Data.ByteString.Lazy as LB
import Data.String
import Data.Text as T
import Data.Word
import GHC.IsList
import Text.Read

import qualified Data.ByteString as SB
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

valueTerminatorChar, nullChar, rowTerminatorChar :: Word8
valueTerminatorChar = 0xFF -- 0xFF
nullChar = 0xFE -- 0xFE
rowTerminatorChar = 0xFD

valueTerminator, nullValue, rowTerminator :: Builder
valueTerminator = word8 valueTerminatorChar
nullValue = word8 nullChar
rowTerminator = word8 rowTerminatorChar

newtype RSVBuilder = RSVBuilder Builder

builder :: RSVBuilder -> Builder
builder (RSVBuilder builder) = builder 

instance Semigroup RSVBuilder where
  (RSVBuilder a) <> (RSVBuilder b) = RSVBuilder $ a <> b

instance Monoid RSVBuilder where
  mempty = RSVBuilder mempty

instance IsString RSVBuilder where
  fromString = RSVBuilder . fromString 

encodeNull :: RSVBuilder
encodeNull = RSVBuilder $ nullValue <> valueTerminator

-- | Encodes a string to RSV
--
-- This function is inherently unsafe because the Haskell @String@
-- type is not guaranteed to have any particular encoding.
encodeStringUnsafe :: String -> RSVBuilder
encodeStringUnsafe s = RSVBuilder $ stringUtf8 s <> valueTerminator

encodeShow :: Show a => a -> RSVBuilder
encodeShow = encodeStringUnsafe . show

encodeText :: Text -> RSVBuilder
encodeText = encodeStringUnsafe . T.unpack

encodeBinary :: SB.ByteString -> RSVBuilder
encodeBinary = RSVBuilder . (<> valueTerminator) . byteString . B64.encode

encodeBinaryLazy :: ByteString -> RSVBuilder
encodeBinaryLazy = RSVBuilder . (<> valueTerminator) . lazyByteString . B64L.encode

class ToRSV a where
  toRSV :: a -> RSVBuilder

instance ToRSV String where
  toRSV = encodeStringUnsafe

instance ToRSV Text where
  toRSV = encodeText

instance ToRSV SB.ByteString where
  toRSV = encodeBinary 

instance ToRSV ByteString where
  toRSV = encodeBinaryLazy 

instance ToRSV Int where
  toRSV = encodeShow

instance ToRSV a => ToRSV (Maybe a) where
  toRSV Nothing = encodeNull
  toRSV (Just a) = toRSV a

encodeRow :: RSVBuilder -> RSVBuilder
encodeRow builder = builder <> RSVBuilder rowTerminator 

class ToRSVRow a where
  toRSVRow :: a -> RSVBuilder

instance (Foldable t, ToRSV a) => ToRSVRow (t a) where
  toRSVRow = encodeRow . foldMap toRSV 

encode :: (Foldable t, ToRSVRow a) => t a -> ByteString
encode = toLazyByteString . builder . foldMap toRSVRow

data DecodeException = UnexpectedEOF | UnexpectedNull | UnpermittedNull | UnexpectedRowTerminator | UnicodeException TEE.UnicodeException | InvalidFormat | MissingRowTerminator deriving Show 
instance Exception DecodeException

type RSVParser a = StateT [Word8] (Either DecodeException) a

parseValue :: RSVParser (Maybe SB.ByteString)
parseValue = toMaybeBS <$> (State.get >>= parseBytes mempty False)
  where
    parseBytes :: [Word8] -> Bool -> [Word8] -> RSVParser [Word8]
    parseBytes _ _ [] = throwError UnexpectedEOF
    parseBytes accum hasNull (w:ws)
      | w == rowTerminatorChar = throwError UnexpectedRowTerminator
      | not (Prelude.null accum) && w == nullChar = throwError UnexpectedNull
      | not hasNull && w == nullChar = parseBytes [w] True ws
      | hasNull && w /= valueTerminatorChar = throwError UnexpectedNull
      | w == valueTerminatorChar = put ws >> return accum
      | otherwise = parseBytes (accum <> [w]) hasNull ws
    toMaybeBS :: [Word8] -> Maybe SB.ByteString
    toMaybeBS bytes
      | bytes == [nullChar] = Nothing
      | otherwise = Just $ SB.pack bytes

formatValue :: (SB.ByteString -> Either DecodeException a) -> Maybe SB.ByteString -> RSVParser a 
formatValue _ Nothing = throwError UnpermittedNull 
formatValue f (Just bs) = State.lift $ f bs

decodeValue :: (SB.ByteString -> Either DecodeException a) -> RSVParser a 
decodeValue f = parseValue >>= formatValue f

decodeText :: RSVParser Text 
decodeText = decodeValue (first UnicodeException . TE.decodeUtf8')

decodeString :: RSVParser String 
decodeString = T.unpack <$> decodeText

decodeBinary :: RSVParser SB.ByteString
decodeBinary = decodeValue $ first (const InvalidFormat) . B64.decode

decodeBinaryLazy :: RSVParser ByteString
decodeBinaryLazy = fromStrict <$> decodeBinary 

decodeRead :: Read a => RSVParser a 
decodeRead = decodeString >>= throwIfNothing . readMaybe
  where
    throwIfNothing :: Maybe a -> RSVParser a
    throwIfNothing Nothing = throwError InvalidFormat
    throwIfNothing (Just a) = return a

permitNull :: RSVParser a -> RSVParser (Maybe a)
permitNull parser = catchError (Just <$> parser) handler
  where
    -- We need `drop 2` because `catchError` resets the state to what it was right
    -- before the parser is executed.
    handler UnpermittedNull = modify (Prelude.drop 2) >> return Nothing 
    handler e = throwError e

class FromRSV a where
  fromRSV :: RSVParser a

instance FromRSV String where
  fromRSV = decodeString

instance FromRSV Text where
  fromRSV = decodeText

instance FromRSV SB.ByteString where
  fromRSV = decodeBinary

instance FromRSV ByteString where
  fromRSV = decodeBinaryLazy

instance FromRSV Int where
  fromRSV = decodeRead

instance FromRSV a => FromRSV (Maybe a) where
  fromRSV = permitNull fromRSV 

class FromRSVRow a where
  fromRSVRow :: RSVParser a

atRowTerminator :: RSVParser Bool
atRowTerminator = gets atRowTerminator' 
  where
    atRowTerminator' :: [Word8] -> Bool
    atRowTerminator' (w:_) = w == rowTerminatorChar
    atRowTerminator' _ = False

assertRowTerminator :: RSVParser () 
assertRowTerminator = State.get >>= checkTerminator
  where
    checkTerminator :: [Word8] -> RSVParser () 
    checkTerminator (w:ws)
      | w == rowTerminatorChar = put ws
      | otherwise = throwError MissingRowTerminator
    checkTerminator _ = throwError MissingRowTerminator 

decodeRow :: RSVParser a -> RSVParser a
decodeRow parser = parser <* assertRowTerminator

decodeList' :: FromRSV a => RSVParser [a]
decodeList' = do
  terminated <- atRowTerminator
  if terminated 
    then modify (Prelude.drop 1) >> return []
    else liftA2 (:) fromRSV decodeList 

decodeList :: (FromRSV (Item l), IsList l) => RSVParser l
decodeList = fromList <$> decodeList'

instance FromRSV a => FromRSVRow [a] where 
  fromRSVRow = decodeList'

decode :: FromRSVRow a => ByteString -> Either DecodeException [a]
decode = evalStateT decode' . LB.unpack
  where
    decode' = do
      end <- gets Prelude.null
      if end
        then return []
        else liftA2 (:) fromRSVRow decode'

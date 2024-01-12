{-# LANGUAGE FlexibleInstances #-}

module Data.RSV (
  encode,
  encodeNull,
  encodeShow,
  encodeString,
  encodeText,
  encodeTextLazy,
  nullValue,
  rowTerminator,
  valueTerminator,
  Row(..),
  Utf8Encodable(..),
  Utf8Encoded
) where

import Data.ByteString.Lazy as LB
import Data.ByteString.Builder
import Data.String
import Data.Text as T

import qualified Data.List as L
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as LT

valueTerminator, nullValue, rowTerminator :: Builder 
valueTerminator = word8 255  -- 0xFF
nullValue = word8 254       -- 0xFE
rowTerminator = word8 253    -- 0xFD

newtype Utf8Encoded = Utf8Encoded Builder 

instance IsString Utf8Encoded where
  fromString = encodeString

instance Semigroup Utf8Encoded where
  (Utf8Encoded a) <> (Utf8Encoded b) = Utf8Encoded $ a <> b

instance Monoid Utf8Encoded where
  mempty = Utf8Encoded mempty

encodeNull :: Utf8Encoded
encodeNull = Utf8Encoded $ nullValue <> valueTerminator

encodeShow :: Show a => a -> Utf8Encoded
encodeShow = encodeString . show

encodeString :: String -> Utf8Encoded
encodeString s = Utf8Encoded $ stringUtf8 s <> valueTerminator

encodeText :: Text -> Utf8Encoded
encodeText = encodeString . T.unpack 

encodeTextLazy :: LT.Text -> Utf8Encoded
encodeTextLazy = encodeString . LT.unpack

class Utf8Encodable v where
  encodeUtf8 :: v -> Utf8Encoded 

instance Utf8Encodable Utf8Encoded where
  encodeUtf8 = id

instance Utf8Encodable Int where
  encodeUtf8 = encodeShow

instance Utf8Encodable Integer where
  encodeUtf8 = encodeShow

instance Utf8Encodable String where
  encodeUtf8 = encodeString

instance Utf8Encodable Text where
  encodeUtf8 = encodeText

instance Utf8Encodable LT.Text where
  encodeUtf8 = encodeTextLazy

instance Utf8Encodable v => Utf8Encodable (Maybe v) where
  encodeUtf8 Nothing = encodeNull
  encodeUtf8 (Just v) = encodeUtf8 v

class Row r where
  values :: r -> [Utf8Encoded]

instance (Foldable t, Utf8Encodable v) => Row (t v) where
  values = foldMap (L.singleton . encodeUtf8)

buildUtf8Encoded :: Utf8Encoded -> Builder
buildUtf8Encoded (Utf8Encoded builder) = builder

buildList :: [Utf8Encoded] -> Builder
buildList encodeds = mconcat (Prelude.map buildUtf8Encoded encodeds) <> rowTerminator

buildRow :: Row r => r -> Builder
buildRow = buildList . values

buildRowAccum :: Row r => r -> Builder -> Builder
buildRowAccum row builder = builder <> buildRow row

encode :: (Foldable t, Row r) => t r -> ByteString
encode = toLazyByteString . Prelude.foldr buildRowAccum mempty 

class Utf8Decodable v where
  decodeUtf8 :: ByteString -> Either TEE.UnicodeException v 

instance Utf8Decodable Text where
  decodeUtf8 = TE.decodeUtf8' . toStrict

instance Utf8Decodable String where
  decodeUtf8 bs = T.unpack <$> decodeUtf8 bs

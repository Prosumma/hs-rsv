{-# LANGUAGE FlexibleContexts, FlexibleInstances, TupleSections #-}

module Data.RSV (
  assertRowTerminator,
  encode,
  encodeNull,
  encodeShow,
  encodeStringUnsafe,
  encodeText,
  encodeRow,
  nullChar,
  parse,
  parse',
  parseBinary,
  parseBinaryLazy,
  parseByteString,
  parseList,
  parseRead,
  parseRow,
  parseValue,
  permitNull,
  rowTerminatorChar,
  throwRSVException,
  tryParse,
  valueTerminatorChar,
  Builder,
  FromRSV(..),
  FromRSVRow(..),
  RSVError,
  RSVException(..),
  RSVParser,
  RSVParseState,
  ToRSV(..),
  ToRSVRow(..)
) where

import Control.Exception
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString.Builder
import Data.ByteString.Lazy as LB hiding (drop)
import Data.Text as T hiding (drop)
import Data.Word
import GHC.IsList
import Text.Read hiding (get, lift)

import qualified Data.ByteString as SB 
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

valueTerminatorChar, nullChar, rowTerminatorChar :: Word8
valueTerminatorChar = 0xFF -- 0xFF
nullChar = 0xFE -- 0xFE
rowTerminatorChar = 0xFD

data RSVException = UnexpectedEOF | UnexpectedNull | UnpermittedNull | UnexpectedRowTerminator | UnicodeException TEE.UnicodeException | InvalidFormat | MissingRowTerminator deriving Show 
instance Exception RSVException 
type RSVError = (Integer, RSVException)
type RSVParseState = (Integer, [Word8])

parseByteString :: RSVParseState -> Either RSVError (RSVParseState, Maybe SB.ByteString)
parseByteString state = second toByteString <$> parseByteString' False mempty state 
  where
    toByteString :: [Word8] -> Maybe SB.ByteString
    toByteString ws
      | ws == [nullChar] = Nothing
      | otherwise = Just $ SB.pack ws
    parseByteString' :: Bool -> [Word8] -> RSVParseState -> Either RSVError (RSVParseState, [Word8])
    parseByteString' _ _ (p, []) = throwError (p, UnexpectedEOF)
    parseByteString' hasNull accum (p, w:ws)
      | w == rowTerminatorChar = throwError (p, UnexpectedRowTerminator)
      | not (Prelude.null accum) && w == nullChar = throwError (p, UnexpectedNull)
      | not hasNull && w == nullChar = parseByteString' True [w] (p + 1, ws)
      | hasNull && w /= valueTerminatorChar = throwError (p - 1, UnexpectedNull)
      | w == valueTerminatorChar = return ((p + 1, ws), accum)
      | otherwise = parseByteString' hasNull (accum <> [w]) (p + 1, ws)

type RSVParser a = StateT RSVParseState (Either RSVError) a

advanceBy :: Int -> RSVParser ()
advanceBy n = modify $ bimap (+ fromIntegral n) (drop n)

throwRSVException :: RSVException -> RSVParser a
throwRSVException e = do
  p <- gets fst
  throwError (p, e)

atRowTerminator :: RSVParser Bool
atRowTerminator = gets snd >>= check 
  where
    check :: [Word8] -> RSVParser Bool
    check (w:_) = return $ w == rowTerminatorChar
    check _ = return False

atParsedRowTerminator :: RSVParser Bool
atParsedRowTerminator = do
  terminated <- atRowTerminator
  when terminated $ advanceBy 1 
  return terminated

assertRowTerminator :: RSVParser ()
assertRowTerminator = do
  terminated <- atParsedRowTerminator
  unless terminated $ throwRSVException MissingRowTerminator

-- | Properly rewinds the state in the case of @InvalidFormat@ or @UnpermittedNull@.
--
-- For all other RSVException types, we want the index position to remain the same
-- as it was when the exception was thrown. But for these two, we want it to be
-- at the position it was when parsing started.
-- 
-- In general, any parser which can fail with @InvalidFormat@ or @UnpermittedNull@ should
-- start with this function. For an example, see @parseText@.
tryParse :: RSVParser a -> RSVParser a
tryParse parser = do 
  state <- get
  case runStateT parser state of
    Left (_, InvalidFormat) -> throwError (fst state, InvalidFormat)
    Left (_, UnpermittedNull) -> throwError (fst state, UnpermittedNull)
    Left e -> throwError e
    Right (a, newState) -> put newState >> return a

parseValue :: RSVParser SB.ByteString 
parseValue = do 
  state <- get
  (newState, sb) <- lift $ parseByteString state
  put newState
  case sb of
    Just sb -> return sb 
    Nothing -> throwError (fst state, UnpermittedNull) 

convertValue :: (SB.ByteString -> Either RSVException a) -> SB.ByteString -> RSVParser a
convertValue convert bs = case convert bs of
  Right a -> return a
  Left e -> throwRSVException e

parseText :: RSVParser Text
parseText = tryParse $ parseValue >>= convertValue (first UnicodeException . TE.decodeUtf8')

parseString :: RSVParser String
parseString = T.unpack <$> parseText

parseRead :: Read a => RSVParser a
parseRead = tryParse $ do
  s <- parseString
  case readMaybe s of
    Just a -> return a
    Nothing -> throwRSVException InvalidFormat

parseBinary :: RSVParser SB.ByteString
parseBinary = tryParse $ parseValue >>= convertValue (first (const InvalidFormat) . B64.decode)

parseBinaryLazy :: RSVParser ByteString
parseBinaryLazy = fromStrict <$> parseBinary

permitNull :: RSVParser a -> RSVParser (Maybe a)
permitNull parser = catchError (Just <$> parser) handler
  where
    -- We need to advance by 2 because `catchError` resets the state
    -- to what it was before parsing.
    handler (_, UnpermittedNull) = advanceBy 2 >> return Nothing
    handler e = throwError e

class FromRSV a where
  fromRSV :: RSVParser a

instance FromRSV Text where
  fromRSV = parseText

instance FromRSV String where
  fromRSV = parseString

instance FromRSV Int where
  fromRSV = parseRead

instance FromRSV SB.ByteString where
  fromRSV = parseBinary

instance FromRSV ByteString where
  fromRSV = parseBinaryLazy

instance FromRSV a => FromRSV (Maybe a) where
  fromRSV = permitNull fromRSV

class FromRSVRow a where
  fromRSVRow :: RSVParser a

parseRow :: RSVParser a -> RSVParser a
parseRow parser = parser <* assertRowTerminator

parseList' :: FromRSV a => RSVParser [a]
parseList' = do
  terminated <- atRowTerminator
  if terminated 
    then advanceBy 1 >> return []
    else liftA2 (:) fromRSV parseList'

parseList :: (FromRSV (Item l), IsList l) => RSVParser l
parseList = fromList <$> parseList'

instance FromRSV a => FromRSVRow [a] where 
  fromRSVRow = parseList'

parse' :: FromRSVRow a => RSVParser [a]
parse' = do
  end <- gets $ Prelude.null . snd
  if end 
    then return []
    else liftA2 (:) fromRSVRow parse'

parse :: FromRSVRow a => ByteString -> Either RSVError [a]
parse = evalStateT parse' . (0,) . LB.unpack

valueTerminator, nullValue, rowTerminator :: Builder
valueTerminator = word8 valueTerminatorChar
nullValue = word8 nullChar
rowTerminator = word8 rowTerminatorChar

-- | Encodes a string to RSV
--
-- This function is inherently unsafe because the Haskell @String@
-- type is not guaranteed to have any particular encoding.
encodeStringUnsafe :: String -> Builder
encodeStringUnsafe s = stringUtf8 s <> valueTerminator

encodeNull :: Builder
encodeNull = nullValue <> valueTerminator

encodeShow :: Show a => a -> Builder
encodeShow = encodeStringUnsafe . show

encodeText :: Text -> Builder
encodeText = encodeStringUnsafe . T.unpack

encodeBinary :: SB.ByteString -> Builder
encodeBinary = byteString . B64.encode

encodeBinaryLazy :: ByteString -> Builder
encodeBinaryLazy = lazyByteString . B64L.encode

class ToRSV a where
  toRSV :: a -> Builder

instance ToRSV String where
  toRSV = encodeStringUnsafe

instance ToRSV Text where
  toRSV = encodeText

instance ToRSV Int where
  toRSV = encodeShow

instance ToRSV SB.ByteString where
  toRSV = encodeBinary

instance ToRSV ByteString where
  toRSV = encodeBinaryLazy

instance ToRSV a => ToRSV (Maybe a) where
  toRSV Nothing = encodeNull
  toRSV (Just a) = toRSV a

encodeRow :: Builder -> Builder
encodeRow = (<> rowTerminator)

class ToRSVRow a where
  toRSVRow :: a -> Builder

instance (Foldable t, ToRSV a) => ToRSVRow (t a) where
  toRSVRow = encodeRow . foldMap toRSV

instance ToRSVRow a => ToRSVRow (Maybe a) where
  toRSVRow Nothing = mempty
  toRSVRow (Just a) = toRSVRow a

encode :: (Foldable t, ToRSVRow a) => t a -> ByteString
encode = toLazyByteString . foldMap toRSVRow
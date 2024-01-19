{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralisedNewtypeDeriving, TypeFamilies #-}

module Data.RSV (
  decodeValue,
  encode,
  encodeRow,
  encodeValue,
  encodeShow,
  encodeStringUnsafe,
  encodeText,
  formatValue,
  newParserState,
  nullChar,
  parse,
  parseRead,
  parseRow,
  parseString,
  parseText,
  parseValue,
  permitNull,
  rowTerminatorChar,
  throwIndexedException,
  valueTerminatorChar,
  FromRow(..),
  FromValue(..),
  IndexedException,
  ParseException(..),
  ParserIndices(..),
  ParserState(..),
  RowParser,
  ToRow(..),
  ToValue(..),
  ValueParser
) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.State (evalStateT, gets, modify, runStateT, MonadState(..), StateT(..))
import Data.Bifunctor
import Data.ByteString.Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString (StrictByteString)
import Data.Text (Text)
import Data.Word
import GHC.IsList
import Text.Printf
import Text.Read hiding (get)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

valueTerminatorChar, nullChar, rowTerminatorChar :: Word8
valueTerminatorChar = 0xFF
nullChar = 0xFE
rowTerminatorChar = 0xFD

data ParserIndices = ParserIndices {
  byteIndex  :: !Integer,
  valueIndex :: !Integer,
  rowIndex   :: !Integer
} deriving (Eq, Show)

advanceByteIndex :: ParserIndices -> ParserIndices
advanceByteIndex (ParserIndices b v r) = ParserIndices (b + 1) v r

rewindByteIndex :: ParserIndices -> ParserIndices
rewindByteIndex (ParserIndices b v r) = ParserIndices (b - 1) v r

resetValueIndex :: ParserIndices -> ParserIndices
resetValueIndex (ParserIndices b _ r) = ParserIndices b b r

resetRowIndex :: ParserIndices -> ParserIndices
resetRowIndex (ParserIndices b _ _) = ParserIndices b b b

data ParserState = ParserState {
  remainingBytes :: [Word8],
  parserIndices :: !ParserIndices
} deriving (Eq, Show)

newParserState :: LazyByteString -> ParserState
newParserState lbs = ParserState (LB.unpack lbs) (ParserIndices 0 0 0)

advanceParserState :: [Word8] -> ParserState -> ParserState
advanceParserState bytes (ParserState _ indices) = ParserState bytes (advanceByteIndex indices)

data ParseException = UnknownError | UnexpectedEOF | UnexpectedNull | UnexpectedRowTerminator | UnpermittedNull | UnicodeError | ConversionError String | MissingRowTerminator deriving (Eq, Show)
type IndexedException = (ParserIndices, ParseException)

newtype ValueParser a = ValueParser (StateT ParserState (Either IndexedException) a)
  deriving (Functor, Applicative, Monad, MonadState ParserState, MonadError IndexedException)

throwIndexedException :: (MonadState ParserState m, MonadError IndexedException m) => ParseException -> m a
throwIndexedException e = do
  indices <- gets parserIndices
  throwError (indices, e)

instance Alternative ValueParser where
  empty = throwIndexedException UnknownError
  (ValueParser lhs) <|> (ValueParser rhs) = ValueParser $ do
    state <- get
    let result = runStateT lhs state
    case result of
      Left _ -> rhs
      Right (a, newState) -> put newState >> return a

decodeValue :: ParserState -> Either IndexedException (Maybe StrictByteString, ParserState)
decodeValue (ParserState bytes indices) = first toStrictByteString <$>
    decodeValue' (ParserState bytes (resetValueIndex indices)) mempty False
  where
    decodeValue' :: ParserState -> [Word8] -> Bool -> Either IndexedException ([Word8], ParserState)
    decodeValue' (ParserState [] indices) _ _ = throwError (indices, UnexpectedEOF)
    decodeValue' state@(ParserState (byte:remainingBytes) indices) accum hasNull
      | byte == rowTerminatorChar = throwError (indices, UnexpectedRowTerminator)
      | not (null accum) && byte == nullChar = throwError (indices, UnexpectedNull)
      | not hasNull && byte == nullChar = decodeValue' (advanceParserState remainingBytes state) [byte] True
      | hasNull && byte /= valueTerminatorChar = throwError (rewindByteIndex indices, UnexpectedNull)
      | byte == valueTerminatorChar = return (accum, advanceParserState remainingBytes state)
      | otherwise = decodeValue' (advanceParserState remainingBytes state) (accum <> [byte]) hasNull
    toStrictByteString :: [Word8] -> Maybe StrictByteString
    toStrictByteString bytes
      | bytes == [nullChar] = Nothing
      | otherwise = Just $ SB.pack bytes

parseValue :: ValueParser StrictByteString
parseValue = do
  result <- gets decodeValue
  case result of
    Left e -> throwError e
    Right (Nothing, newState) -> put newState >> throwIndexedException UnpermittedNull
    Right (Just bs, newState) -> put newState >> return bs

parseText :: ValueParser Text
parseText = do
  sbs <- parseValue
  case TE.decodeUtf8' sbs of
    Left _ -> throwIndexedException UnicodeError
    Right t -> return t

parseString :: ValueParser String
parseString = T.unpack <$> parseText

parseRead :: Read a => ValueParser a
parseRead = do
  s <- parseString
  case readMaybe s of
    Nothing -> throwIndexedException $ ConversionError $ printf "Could not convert string %s to desired type." s
    Just a -> return a

parseList :: FromValue a => ValueParser [a]
parseList = do 
  done <- isAtRowTerminator
  if done
    then return []
    else liftA2 (:) fromValue parseList
  where
    isAtRowTerminator = gets remainingBytes >>= checkRowTerminator 
    checkRowTerminator [] = throwIndexedException UnexpectedEOF 
    checkRowTerminator (byte:_)
      | byte == rowTerminatorChar = return True
      | otherwise = return False

formatValue :: (v -> Either String a) -> v -> ValueParser a
formatValue convert value = case convert value of
  Left error -> throwIndexedException (ConversionError error)
  Right a -> return a

permitNull :: ValueParser a -> ValueParser (Maybe a)
permitNull parser = catchError (Just <$> parser) handleError
  where
    handleError (_, UnpermittedNull) = modify advanceBy2 >> return Nothing
    handleError e = throwError e
    advanceBy2 (ParserState bytes (ParserIndices b v r)) = ParserState (drop 2 bytes) (ParserIndices (b + 2) v r)

class FromValue a where
  fromValue :: ValueParser a

instance FromValue Text where
  fromValue = parseText

instance FromValue Int where
  fromValue = parseRead

instance FromValue a => FromValue (Maybe a) where
  fromValue = permitNull fromValue

newtype RowParser a = RowParser (StateT ParserState (Either IndexedException) a)
  deriving (Functor, Applicative, Monad, MonadState ParserState, MonadError IndexedException)

instance Alternative RowParser where
  empty = throwIndexedException UnknownError
  (RowParser lhs) <|> (RowParser rhs) = RowParser $ do
    state <- get
    let result = runStateT lhs state
    case result of
      Left _ -> rhs
      Right (a, newState) -> put newState >> return a

parseRow :: ValueParser a -> RowParser a
parseRow (ValueParser parser) = do
  state <- gets resetRow
  case runStateT parser state of
    Left e -> throwError e
    Right (result, newState) -> put newState >> finish result (remainingBytes newState)
  where
    resetRow (ParserState bytes indices) = ParserState bytes (resetRowIndex indices)
    finish _ [] = throwIndexedException UnexpectedEOF
    finish a (byte:bytes)
      | byte == rowTerminatorChar = modify (advanceParserState bytes) >> return a
      | otherwise = throwIndexedException MissingRowTerminator

class FromRow a where
  fromRow :: RowParser a

instance (FromValue a, FromValue b) => FromRow (a, b) where
  fromRow = parseRow $ (,) <$> fromValue <*> fromValue

instance FromValue a => FromRow [a] where
  fromRow = parseRow parseList

parse :: FromRow a => LazyByteString -> Either IndexedException [a]
parse = evalStateT (parse' fromRow) . newParserState
  where
    parse' (RowParser parser) = do 
      isAtEnd <- atEnd
      if isAtEnd
        then return []
        else liftA2 (:) parser (parse' (RowParser parser))
    atEnd = gets $ null . remainingBytes

encodeValue :: Builder -> Builder
encodeValue builder = builder <> word8 valueTerminatorChar

encodeNull :: Builder
encodeNull = encodeValue $ word8 nullChar

encodeStringUnsafe :: String -> Builder
encodeStringUnsafe = encodeValue . stringUtf8

encodeText :: Text -> Builder
encodeText = encodeStringUnsafe . T.unpack 

encodeShow :: Show a => a -> Builder
encodeShow = encodeStringUnsafe . show

class ToValue a where
  toValue :: a -> Builder

instance ToValue String where
  toValue = encodeStringUnsafe

instance ToValue Text where
  toValue = encodeText

instance ToValue Int where
  toValue = encodeShow

instance ToValue a => ToValue (Maybe a) where
  toValue Nothing = encodeNull
  toValue (Just a) = toValue a

encodeRow :: Builder -> Builder
encodeRow builder = builder <> word8 rowTerminatorChar

class ToRow a where
  toRow :: a -> Builder

instance (Foldable t, ToValue a) => ToRow (t a) where
  toRow = encodeRow . foldMap toValue 

encode :: (Foldable t, ToRow a) => t a -> LazyByteString 
encode = toLazyByteString . foldMap toRow

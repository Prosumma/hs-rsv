{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralisedNewtypeDeriving, OverloadedStrings, TypeFamilies #-}

module Data.RSV (
  convertValue,
  decodeValue,
  encode,
  encodeNull,
  encodeRow,
  encodeValue,
  encodeShow,
  encodeStringUnsafe,
  encodeText,
  encodeWith,
  foldApp,
  newParserState,
  nullChar,
  parse,
  parseRead,
  parseRow,
  parseString,
  parseText,
  parseValue,
  parseWith,
  permitNull,
  rowTerminatorChar,
  throwIndexedException,
  valueTerminatorChar,
  FromRow(..),
  FromValue(..),
  IndexedException,
  ParseException(..),
  ParserConfig(..),
  ParserIndices(..),
  ParserState(..),
  RowParser,
  ToRow(..),
  ToValue(..),
  ValueParser,
  (<+>)
) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Reader (asks, runReader, MonadReader(..), Reader)
import Control.Monad.State (gets, modify, MonadState(..))
import Control.Monad.Trans.RWS (evalRWST, runRWST, RWST)
import Data.Bifunctor
import Data.ByteString.Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString (StrictByteString)
import Data.Default
import Data.Set (member, Set)
import Data.Text (Text)
import Data.Word
import Text.Printf
import Text.Read hiding (get)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

askGet :: (MonadReader env m, MonadState state m) => m (env, state)
askGet = do
  env <- ask
  state <- get
  return (env, state)

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

data ParserConfig = ParserConfig {
  trueValue   :: !Text,
  falseValue  :: !Text,
  trueValues  :: !(Set Text),
  falseValues :: !(Set Text)
}

instance Default ParserConfig where
  def = ParserConfig "true" "false" (Set.fromList ["true", "t", "yes", "y", "1"]) (Set.fromList ["false", "f", "no", "n", "0"])

data ParseException = UnknownError | UnexpectedEOF | UnexpectedNull | UnexpectedRowTerminator | UnpermittedNull | UnicodeError | ConversionError String | MissingRowTerminator deriving (Eq, Show)
type IndexedException = (ParserIndices, ParseException)

newtype ValueParser a = ValueParser (RWST ParserConfig () ParserState (Either IndexedException) a)
  deriving (Functor, Applicative, Monad, MonadReader ParserConfig, MonadState ParserState, MonadError IndexedException)

throwIndexedException :: (MonadState ParserState m, MonadError IndexedException m) => ParseException -> m a
throwIndexedException e = do
  indices <- gets parserIndices
  throwError (indices, e)

instance Alternative ValueParser where
  empty = throwIndexedException UnknownError
  (ValueParser lhs) <|> (ValueParser rhs) = ValueParser $ do
    (config, state) <- askGet
    let result = runRWST lhs config state
    case result of
      Left _ -> rhs
      Right (a, newState, _) -> put newState >> return a

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

convertValue :: (v -> Either String a) -> v -> ValueParser a
convertValue convert value = case convert value of
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

instance FromValue Integer where
  fromValue = parseRead

instance FromValue Bool where
  fromValue = do
    config <- ask
    parseText >>= convertValue (toBool config)
    where
      toBool :: ParserConfig -> Text -> Either String Bool
      toBool config text
        | lowerText `member` trueValues config = return True 
        | lowerText `member` falseValues config = return False
        | otherwise = throwError $ printf "The value '%s' is not a valid value for Bool. Use a newtype wrapper." (show text) 
        where
          lowerText = T.toLower text

instance FromValue a => FromValue (Maybe a) where
  fromValue = permitNull fromValue

newtype RowParser a = RowParser (RWST ParserConfig () ParserState (Either IndexedException) a)
  deriving (Functor, Applicative, Monad, MonadReader ParserConfig, MonadState ParserState, MonadError IndexedException)

instance Alternative RowParser where
  empty = throwIndexedException UnknownError
  (RowParser lhs) <|> (RowParser rhs) = RowParser $ do
    (config, state) <- askGet
    let result = runRWST lhs config state 
    case result of
      Left _ -> rhs
      Right (a, newState, _) -> put newState >> return a

parseRow :: ValueParser a -> RowParser a
parseRow (ValueParser parser) = do
  config <- ask
  state <- gets resetRow
  case runRWST parser config state of
    Left e -> throwError e
    Right (result, newState, _) -> put newState >> finish result (remainingBytes newState)
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

parseWith :: FromRow a => ParserConfig -> LazyByteString -> Either IndexedException [a]
parseWith config lbs = fst <$> evalRWST (parse' fromRow) config (newParserState lbs)
  where
    parse' (RowParser parser) = do 
      isAtEnd <- atEnd
      if isAtEnd
        then return []
        else liftA2 (:) parser (parse' (RowParser parser))
    atEnd = gets $ null . remainingBytes

parse :: FromRow a => LazyByteString -> Either IndexedException [a]
parse = parseWith def

type Encoder = Reader ParserConfig Builder

infixr 6 <+>

(<+>) :: (Applicative f, Monoid m) => f m -> f m -> f m 
a <+> b = liftA2 (<>) a b

foldApp :: (Foldable t, Monoid m, Applicative f) => (a -> f m) -> t a -> f m
foldApp f = foldr (\a accum -> f a <+> accum) (pure mempty)

encodeValue :: Encoder -> Encoder 
encodeValue encoder = encoder <+> pure (word8 valueTerminatorChar)

encodeBuilder :: Builder -> Encoder
encodeBuilder = encodeValue . pure

encodeNull :: Encoder
encodeNull = encodeBuilder $ word8 nullChar 

encodeStringUnsafe :: String -> Encoder
encodeStringUnsafe = encodeBuilder . stringUtf8 

encodeText :: Text -> Encoder
encodeText = encodeStringUnsafe . T.unpack

encodeShow :: Show a => a -> Encoder 
encodeShow = encodeStringUnsafe . show

class ToValue a where
  toValue :: a -> Encoder

instance ToValue Text where
  toValue = encodeText

instance ToValue String where
  toValue = encodeStringUnsafe

instance ToValue Int where
  toValue = encodeShow

instance ToValue Bool where
  toValue True = asks trueValue >>= encodeText 
  toValue False = asks falseValue >>= encodeText

instance ToValue a => ToValue (Maybe a) where
  toValue Nothing = encodeNull
  toValue (Just a) = toValue a

encodeRow :: Encoder -> Encoder
encodeRow = (<+> pure (word8 rowTerminatorChar))

class ToRow a where
  toRow :: a -> Encoder

instance (Foldable t, ToValue a) => ToRow (t a) where
  toRow = encodeRow . foldApp toValue

encodeWith :: (Foldable t, ToRow a) => ParserConfig -> t a -> LazyByteString
encodeWith config rows = toLazyByteString $ runReader (foldApp toRow rows) config

encode :: (Foldable t, ToRow a) => t a -> LazyByteString
encode = encodeWith def

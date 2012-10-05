{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Database.Memcached.Commands (
  MemcachedT,

  Command(..),
  Response(..),
  Error(..),

  parseCommand,
  fromResponse,

  execCommand,
  ) where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as S
import qualified Data.Char                        as C
import           Data.Monoid
import qualified Data.Text                        as T
import           Data.Word
import           Prelude                          hiding (lookup)
import Data.Maybe

import Database.Curry as Curry

-- protocol: <https://github.com/memcached/memcached/blob/master/doc/protocol.txt>

type MemcachedT m = DBMT S.ByteString m

data Command
    -- Storage
  = Set     S.ByteString Word32 Int S.ByteString
  | Add     S.ByteString Word32 Int S.ByteString
  | Replace S.ByteString Word32 Int S.ByteString
  | Append  S.ByteString Word32 Int S.ByteString
  | Prepend S.ByteString Word32 Int S.ByteString
  | Cas     S.ByteString Word32 Int Word64 S.ByteString

    -- Retrieval
  | Get  [S.ByteString]
  | Gets [S.ByteString]

    -- Deletion
  | Delete S.ByteString

    -- Other
  | Incr S.ByteString Word64
  | Decr S.ByteString Word64
  | Touch S.ByteString Int
  -- | Slabs
  | Stats
  | FlushAll
  | Version
  | Verbosity Int
  | Quit
  deriving (Show)

data Response
  = Stored
  | NotStored
  | Exists
  | NotFound
  | Values [(S.ByteString, S.ByteString)]
  | Deleted
  | Value S.ByteString

data Error
  = Error
  | ClientError T.Text
  | ServerError T.Text

-- memcached text protocol parser

parseCommand :: A.Parser Command
parseCommand =
      Set     <$> "set "     .*> key <*. " " <*> decimal <*. " " <*> decimal <*. " " <*> bytes
  <|> Add     <$> "add "     .*> key <*. " " <*> decimal <*. " " <*> decimal <*. " " <*> bytes
  <|> Replace <$> "replace " .*> key <*. " " <*> decimal <*. " " <*> decimal <*. " " <*> bytes
  <|> Append  <$> "append "  .*> key <*. " " <*> decimal <*. " " <*> decimal <*. " " <*> bytes
  <|> Prepend <$> "prepend " .*> key <*. " " <*> decimal <*. " " <*> decimal <*. " " <*> bytes

  <|> do cas <- Cas <$> "cas " .*> key <*. " " <*> decimal <*. " " <*> decimal <*. " "
         len <- decimal <*. " "
         cas <$> decimal <*. crlf <*> A.take len <*. crlf

  <|> Get  <$> "get"  .*> some (" " .*> key) <*. crlf
  <|> Gets <$> "gets" .*> some (" " .*> key) <*. crlf

  <|> Delete <$> "delete " .*> key <*. crlf

  <|> Incr  <$> "incr "  .*> key <*> " " .*> decimal <*. crlf
  <|> Decr  <$> "decr "  .*> key <*> " " .*> decimal <*. crlf
  <|> Touch <$> "touch " .*> key <*> " " .*> decimal <*. crlf

  <|> pure Stats     <*. "stats"     <*. crlf
  <|> pure FlushAll  <*. "flush_all" <*. crlf
  <|> pure Version   <*. "version"   <*. crlf
  <|> pure Verbosity <*. "verbosity" <*> decimal <*. crlf
  <|> pure Quit      <*. "quit"      <*. crlf

  where
    key = A.takeWhile1 $ \c -> not (C.isSpace c || C.isControl c)
    bytes = decimal <*. crlf >>= \len -> A.take len <*. crlf

fromResponse :: Response -> Builder
fromResponse resp = case resp of
  Stored ->
    fromByteString "STORED\r\n"
  NotStored ->
    fromByteString "NOT_STORED\r\n"
  Exists ->
    fromByteString "EXISTS\r\n"
  NotFound ->
    fromByteString "NOT_FOUND\r\n"

  Values kvs -> mconcat
    [ fromByteString "VALUE " <> fromByteString key <> fromByteString " 0 " <>
      fromString (show $ S.length val) <> fromByteString "\r\n" <>
      fromByteString val <> fromByteString "\r\n"
    | (key, val) <- kvs
    ] <> fromByteString "END\r\n"

  Deleted ->
    fromByteString "DELETED\r\n"

  Value bs ->
    fromByteString bs

crlf :: S.ByteString
crlf = "\r\n"

-----

execCommand :: Command -> MemcachedT IO Response
execCommand req = transaction $ case req of
  Set key _flags _exptime val -> do
    Curry.insert key val
    return Stored

  Add key _flags _exptime val -> do
    Curry.lookup key >>= \mb -> case mb of
      Nothing -> do
        Curry.insert key val
        return Stored
      Just _ ->
        return NotStored

  Replace key _flags _exptime val -> do
    Curry.lookup key >>= \mb -> case mb of
      Nothing ->
        return NotStored
      Just _ -> do
        Curry.insert key val
        return Stored

  Append key _flags _exptime val -> do
    Curry.insertWith (\new old -> old <> new) key val
    return Stored

  Prepend key _flags _exptime val -> do
    Curry.insertWith (\new old -> new <> old) key val
    return Stored

  Get ks ->
    Values . catMaybes . zipWith (\k v -> (k, ) <$> v) ks <$> mapM Curry.lookup ks

  Delete key -> do
    Curry.lookup key >>= \mb -> case mb of
      Nothing ->
        return NotFound
      Just _ -> do
        Curry.delete key
        return Deleted

  Incr key val -> incr key (+ val)
  Decr key val -> incr key (subtract val)

  where
    incr key f = do
      Curry.lookup key >>= \mb -> case mb of
        Nothing ->
          return NotFound
        Just bs ->
          case S.readInt bs of
            Just (cur, "") -> do
              let next = S.pack $ show (f $ fromIntegral cur)
              Curry.insert key next
              return $ Value next
            _ ->
              return NotFound

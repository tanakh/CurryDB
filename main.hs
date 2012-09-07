{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Trans
import           Data.Attoparsec.ByteString.Char8 as A
import           Data.Attoparsec.Types
import qualified Data.ByteString.Char8            as S
import qualified Data.Char                        as C
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import qualified Data.Text                        as T
import           Data.Word
import           Network                          (withSocketsDo)
import           Prelude                          hiding (lookup)
import Data.Maybe
import Data.Monoid
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Char8

import           Database.KVS as KVS

-- protocol: <https://github.com/memcached/memcached/blob/master/doc/protocol.txt>

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

parseResponse :: A.Parser Response
parseResponse = undefined

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

---

server :: IO ()
server = withSocketsDo $ runDBMT $ do
  runTCPServer (ServerSettings 3333 "*") app

app :: Application (DBMT IO)
app src sink =
  src $$ conduitParser parseCommand =$ awaitForever p =$ sink
  where
    p (_range, req) = do
      liftIO $ putStrLn $ "server: " ++ show req
      resp <- lift $ process req
      yield . toByteString . fromResponse $ resp

process :: Command -> DBMT IO Response
process req = case req of
  Set key _flags _exptime val -> do
    KVS.insert key val
    return Stored

  Add key _flags _exptime val -> do
    KVS.lookup key >>= \mb -> case mb of
      Nothing -> do
        KVS.insert key val
        return Stored
      Just _ ->
        return NotStored

  Replace key _flags _exptime val -> do
    KVS.lookup key >>= \mb -> case mb of
      Nothing ->
        return NotStored
      Just _ -> do
        KVS.insert key val
        return Stored

  Append key _flags _exptime val -> do
    KVS.insertWith (\new old -> old <> new) key val
    return Stored

  Prepend key _flags _exptime val -> do
    KVS.insertWith (\new old -> new <> old) key val
    return Stored

  Get ks ->
    Values . catMaybes . zipWith (\k v -> (k, ) <$> v) ks <$> mapM KVS.lookup ks

  Delete key -> do
    KVS.lookup key >>= \mb -> case mb of
      Nothing ->
        return NotFound
      Just _ -> do
        KVS.delete key
        return Deleted

  Incr key val -> incr key (+ val)
  Decr key val -> incr key (subtract val)

  where
    incr key f = transaction $ do
      KVS.lookup key >>= \mb -> case mb of
        Nothing ->
          return NotFound
        Just bs ->
          case S.readInt bs of
            Just (cur, "") -> do
              let next = S.pack $ show (f $ fromIntegral cur)
              KVS.insert key next
              return $ Value next
            _ ->
              return NotFound

main :: IO ()
main = server `race_` client

client :: IO ()
client = withSocketsDo $ do
  runTCPClient (ClientSettings 3333 "localhost") $ \src sink ->
    src $$ cond =$ sink
  where
    cond = do
      resp1 <- yield "set foo 0 0 3\r\nbar\r\n" >> await
      liftIO $ putStrLn $ "client : " ++ show resp1

      resp2 <- yield "add foo 0 0 3\r\nbaz\r\n" >> await
      liftIO $ putStrLn $ "client : " ++ show resp2

      resp3 <- yield "get foo hoge\r\n" >> await
      liftIO $ putStrLn $ "client : " ++ show resp3

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module Database.Redis.Commands (
  process,
  ) where

import           Control.Applicative
import           Control.Monad.Trans   (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as S
import           Data.Conduit
import qualified Data.Conduit.List     as CL
import           Data.Foldable
import qualified Data.HashSet          as HS
import           Data.Maybe
import qualified Data.Sequence         as Seq

import           Database.KVS          as KVS
import           Database.Redis.Types

type RedisCommand =
  forall m. (Functor m, Applicative m, MonadIO m)
  => RedisT m Reply

ping :: RedisCommand
ping = return $ StatusReply "PONG"
{-# INLINE ping #-}

set :: S.ByteString -> S.ByteString -> RedisCommand
set key val = do
  KVS.insert key (VString val)
  return replyOK

mset :: [S.ByteString] -> RedisCommand
mset ss = transaction $ go ss >> return replyOK where
  go (key: val: rest) = do
    KVS.insert key (VString val)
    go rest
  go _ = return ()

get :: S.ByteString -> RedisCommand
get key = do
  x <- KVS.lookup key
  return $ case x of
    Just (VString val) -> BulkReply $ Just val
    Nothing -> BulkReply Nothing
    _ -> typeErr

incr, decr :: S.ByteString -> RedisCommand
incr = modInt succ
decr = modInt pred

modInt :: (Int -> Int) -> S.ByteString -> RedisCommand
modInt f key = transaction $ do
  x <- KVS.lookup key
  case x of
    Just (VString (toInt -> Just (f -> val))) -> do
      KVS.insert key $ toVString val
      return $ IntReply val
    Nothing -> do
      KVS.insert key $ toVString $ f 0
      return $ IntReply $ f 0
    Just (VString _) -> do
      return notIntErr
    _ ->
      return typeErr

lpush :: S.ByteString -> [S.ByteString] -> RedisCommand
lpush key vals = transaction $ do
  x <- KVS.lookup key
  case x of
    Just (VList ls) -> do
      KVS.insert key $ VList $ foldl' (\ys y -> y Seq.<| ys) ls vals
      return $ IntReply $ Seq.length ls + length vals
    Nothing -> do
      KVS.insert key $ VList $ foldl' (\ys y -> y Seq.<| ys) Seq.empty vals
      return $ IntReply $ length vals
    _ ->
      return typeErr

lpop :: S.ByteString -> RedisCommand
lpop key = transaction $ do
  x <- fromMaybe (VList Seq.empty) <$> KVS.lookup key
  case x of
    VList ls -> do
      case Seq.viewl ls of
        val Seq.:< rest -> do
          KVS.insert key $ VList rest
          return $ BulkReply $ Just val
        _ ->
          return $ BulkReply Nothing
    _ ->
      return typeErr

lrange :: S.ByteString -> S.ByteString -> S.ByteString -> RedisCommand
lrange key sstart sstop = do
  x <- fromMaybe (VList Seq.empty) <$> KVS.lookup key
  case (x, toInt sstart, toInt sstop) of
    (VList ss, Just start, Just stop) -> do
      return
        $ MultiBulkReply $ Just $ map Just $ toList
        $ Seq.take (stop - start) $ Seq.drop start ss
    _ ->
      return typeErr

sadd :: S.ByteString -> [S.ByteString] -> RedisCommand
sadd key vals = transaction $ do
  x <- fromMaybe (VSet HS.empty) <$> KVS.lookup key
  case x of
    VSet ss -> do
      let nss = foldl' (flip HS.insert) ss vals
      KVS.insert key $ VSet nss
      return $ IntReply $ HS.size nss - HS.size ss
    _ ->
      return typeErr

spop :: S.ByteString -> RedisCommand
spop key = transaction $ do
  x <- fromMaybe (VSet HS.empty) <$> KVS.lookup key
  case x of
    VSet ss -> do
      case HS.toList ss of
        (arb:_) -> do
          KVS.insert key $ VSet $ HS.delete arb ss
          return $ BulkReply $ Just arb
        _ -> do
          return $ BulkReply Nothing
    _ ->
      return typeErr

-----

toInt :: S.ByteString -> Maybe Int
toInt bs =
  case S.readInt bs of
    Just (n, "") -> Just n
    _ -> Nothing

toVString :: Int -> Value
toVString = VString . S.pack . show

replyOK, typeErr, notIntErr :: Reply
replyOK = StatusReply "OK"
typeErr = ErrorReply "ERR Operation against a key holding the wrong kind of value"
notIntErr = ErrorReply "ERR value is not an integer or out of range"

-----

process :: (Functor m, Applicative m, MonadIO m)
           => GInfConduit (a, Request) (RedisT m) Reply
process = CL.mapM $ \(_pos_range, req) -> f req where
  f req = case req of
    Request ["PING"] -> ping

    Request ["SET", key, val] -> set key val
    Request ("MSET": args) -> mset args
    Request ["GET", key] -> get key
    Request ["INCR", key] -> incr key
    Request ["DECR", key] -> decr key

    Request ("LPUSH": key: vals) -> lpush key vals
    Request ["LPOP", key] -> lpop key
    Request ["LRANGE", key, start, stop] -> lrange key start stop

    Request ("SADD": key: vals) -> sadd key vals
    Request ["SPOP", key] -> spop key

    _ -> do
      liftIO $ print req
      return $ ErrorReply "Bad Request"
{-# INLINE process #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module Database.Redis.Commands (
  process,
  ) where

import           Control.Applicative
import           Control.Monad.Trans   (MonadIO)
import qualified Data.ByteString.Char8 as S
import           Data.Conduit
import qualified Data.Conduit.List     as CL
import qualified Data.Sequence as Seq
import Data.List

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
  x <- KVS.lookup key
  case x of
    Just (VList (Seq.viewl -> val Seq.:< rest)) -> do
      KVS.insert key $ VList rest
      return $ BulkReply $ Just val
    Nothing -> do
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
    Request ["GET", key] -> get key
    Request ["INCR", key] -> incr key
    Request ["DECR", key] -> decr key

    Request ("LPUSH": key: vals) -> lpush key vals
    Request ["LPOP", key] -> lpop key

    Request ("MSET": args) -> mset args

    _ -> return $ ErrorReply "Bad Request"
{-# INLINE process #-}

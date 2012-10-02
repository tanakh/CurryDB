{-# LANGUAGE DeriveDataTypeable #-}

module Database.Redis.Types (
  RedisT,
  Value(..),Score, SortedSet,
  Request(..), Reply(..),
  ) where

import           Control.Applicative
import           Data.Binary
import qualified Data.ByteString     as S
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import           Data.Int
import qualified Data.Sequence       as Seq
import qualified Data.Set            as Set

import           Database.Curry

type RedisT m = DBMT Value m

data Value
  = VString    {-# UNPACK #-} !S.ByteString
  | VList                     !(Seq.Seq S.ByteString)
  | VSet                      !(HS.HashSet S.ByteString)
  | VHash                     !(HMS.HashMap S.ByteString S.ByteString)
  | VSortedSet {-# UNPACK #-} !SortedSet

type Score = Int32

type SortedSet = (Set.Set (Score, S.ByteString), HMS.HashMap S.ByteString Score)

data Request
  = Request [S.ByteString]
  deriving (Show)

data Reply
  = StatusReply    {-# UNPACK #-} !S.ByteString
  | ErrorReply     {-# UNPACK #-} !S.ByteString
  | IntReply       {-# UNPACK #-} !Int
  | BulkReply                     !(Maybe S.ByteString)
  | MultiBulkReply                !(Maybe [Maybe S.ByteString])
  deriving (Show)

instance Binary Value where
  put (VString bs)    = put (0 :: Word8) >> put bs
  put (VList ls)      = put (1 :: Word8) >> put ls
  put (VSet ss)       = put (2 :: Word8) >> put (HS.toList ss)
  put (VHash ha)      = put (3 :: Word8) >> put ha
  put (VSortedSet ss) = put (4 :: Word8) >> put ss

  get = get >>= \tag -> case (tag :: Word8) of
    0 -> VString            <$> get
    1 -> VList              <$> get
    2 -> VSet . HS.fromList <$> get
    3 -> VHash              <$> get
    4 -> VSortedSet         <$> get
    _ -> fail "data corrupted"

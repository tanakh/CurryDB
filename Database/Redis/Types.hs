module Database.Redis.Types (
  RedisT,
  Value(..),Score, SortedSet,
  Request(..), Reply(..),
  ) where

import qualified Data.ByteString     as S
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import           Data.Int
import qualified Data.Sequence       as Seq
import qualified Data.Set            as Set

import           Database.KVS

type RedisT m = DBMT Value m

data Value
  = VString    S.ByteString
  | VList      (Seq.Seq S.ByteString)
  | VSet       (HS.HashSet S.ByteString)
  | VHash      (HMS.HashMap S.ByteString S.ByteString)
  | VSortedSet SortedSet

type Score = Int32

type SortedSet = (Set.Set (Score, S.ByteString), HMS.HashMap S.ByteString Score)

data Request
  = Request [S.ByteString]
  deriving (Show)

data Reply
  = StatusReply S.ByteString
  | ErrorReply S.ByteString
  | IntReply Int
  | BulkReply (Maybe S.ByteString)
  | MultiBulkReply (Maybe [Maybe S.ByteString])
  deriving (Show)

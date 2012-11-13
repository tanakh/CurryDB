{-# LANGUAGE OverloadedStrings #-}

module Database.Curry.HashMap (
  HashMap, Key,
  new,
  load, save,
  insert, insertWith,
  delete,
  lookup,
  keys,
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Exception      as E
import           Data.Binary
import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import qualified Data.HashMap.Strict    as HMS
import           Data.Maybe
import qualified Filesystem             as FS
import qualified Filesystem.Path        as FP
import           System.IO

import           Database.Curry.Binary  ()

import           Prelude                hiding (lookup)

type Key = S.ByteString

newtype HashMap v = HashMap { unHM :: TVar (HMS.HashMap Key v) }

new :: IO (HashMap v)
new = HashMap <$> newTVarIO HMS.empty

load :: Binary v => FP.FilePath -> HashMap v -> IO ()
load path hm = do
  v <- decode . L.fromChunks . (\x -> [x]) <$> FS.readFile path
  _ <- E.evaluate v -- force and raise exception when data is corrupted.
  atomically $ writeTVar (unHM hm) v

save :: Binary v => FP.FilePath -> HashMap v -> IO ()
save path hm = do
  tbl <- readTVarIO (unHM hm)
  atomicWriteFile path tbl

op :: (HMS.HashMap Key v -> HMS.HashMap Key v) -> HashMap v -> STM ()
op f hm = modifyTVar' (unHM hm) f
{-# INLINE op #-}

insert :: Key -> v -> HashMap v -> STM ()
insert = insertWith const
{-# INLINE insert #-}

insertWith :: (v -> v -> v) -> Key -> v -> HashMap v -> STM ()
insertWith f k v = op $ HMS.insertWith f k v
{-# INLINE insertWith #-}

delete :: Key -> HashMap v -> STM ()
delete k = op $ HMS.delete k
{-# INLINE delete #-}

lookup :: Key -> HashMap v -> STM (Maybe v)
lookup k hm = HMS.lookup k <$> readTVar (unHM hm)
{-# INLINE lookup #-}

keys :: HashMap v -> STM [Key]
keys hm = HMS.keys <$> readTVar (unHM hm)
{-# INLINE keys #-}

--

atomicWriteFile :: Binary b => FP.FilePath -> b -> IO ()
atomicWriteFile path b = do
  let tmpPath = path FP.<.> "tmp"
  FS.withFile tmpPath WriteMode $ \h ->
    L.hPut h $ encode b
  -- rename operation is atomic
  FS.rename tmpPath path

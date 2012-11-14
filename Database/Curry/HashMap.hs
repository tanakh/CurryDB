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
import           Control.Monad
import qualified Data.Array.MArray      as MA
import           Data.Binary
import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import           Data.Hashable
import qualified Data.HashMap.Strict    as HMS
import           Data.Maybe
import qualified Filesystem             as FS
import qualified Filesystem.Path        as FP
import           System.IO

import           Database.Curry.Binary  ()

import           Prelude                hiding (lookup)

type Key = S.ByteString

newtype HashMap v =
  HashMap { unHM :: TArray Int (HMS.HashMap Key v) }

bucketNum :: Int
bucketNum = 32

bucketIx :: Key -> Int
bucketIx key = hash key `mod` bucketNum

new :: IO (HashMap v)
new = atomically $ do
  HashMap <$> MA.newArray (0, bucketNum - 1) HMS.empty

load :: Binary v => FP.FilePath -> HashMap v -> IO ()
load path hm = do
  vs <- decode . L.fromChunks . (\x -> [x]) <$> FS.readFile path
  _ <- E.evaluate vs -- force and raise exception when data is corrupted.
  when (length vs /= bucketNum) $
    fail "load: data corrupted"
  atomically $ do
    forM_ (zip [0..] vs) $ \(ix, v) -> do
      MA.writeArray (unHM hm) ix v

save :: Binary v => FP.FilePath -> HashMap v -> IO ()
save path hm = do
  vs <- atomically $ forM_ [0 .. bucketNum - 1] $ \ix -> MA.readArray (unHM hm) ix
  atomicWriteFile path vs

insert :: Key -> v -> HashMap v -> STM ()
insert = insertWith const
{-# INLINE insert #-}

insertWith :: (v -> v -> v) -> Key -> v -> HashMap v -> STM ()
insertWith f k v hm = do
  let bix = bucketIx k
  bkt <- MA.readArray (unHM hm) bix
  MA.writeArray (unHM hm) bix $! HMS.insertWith f k v bkt
{-# INLINE insertWith #-}

delete :: Key -> HashMap v -> STM ()
delete k hm = do
  let bix = bucketIx k
  bkt <- MA.readArray (unHM hm) bix
  MA.writeArray (unHM hm) bix $! HMS.delete k bkt
{-# INLINE delete #-}

lookup :: Key -> HashMap v -> STM (Maybe v)
lookup k hm = do
  let bix = bucketIx k
  HMS.lookup k <$> MA.readArray (unHM hm) bix
{-# INLINE lookup #-}

keys :: HashMap v -> STM [Key]
keys hm = do
  bkts <- forM [0 .. bucketNum - 1] $ \ix ->
    HMS.keys <$> MA.readArray (unHM hm) ix
  return $ concat bkts
{-# INLINE keys #-}

--

atomicWriteFile :: Binary b => FP.FilePath -> b -> IO ()
atomicWriteFile path b = do
  let tmpPath = path FP.<.> "tmp"
  FS.withFile tmpPath WriteMode $ \h ->
    L.hPut h $ encode b
  -- rename operation is atomic
  FS.rename tmpPath path

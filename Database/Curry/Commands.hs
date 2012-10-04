{-# LANGUAGE BangPatterns #-}

module Database.Curry.Commands (
  -- Commands
  insert,
  insertWith,
  delete,
  Database.Curry.Commands.lookup,
  lookupDefault,
  keys,

  -- Transaction
  transaction,
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Identity
import qualified Data.ByteString              as S
import           Data.Conduit
import           Data.Default
import qualified Data.HashMap.Strict          as HMS
import           Data.Lens
import           Data.Maybe

import           Database.Curry.Types

insert :: S.ByteString -> v -> DBMS v ()
insert !key !val = do
  table <- access dbmTable
  liftSTM $ modifyTVar' table $ HMS.insert key val
  update
{-# INLINE insert #-}

insertWith :: (v -> v -> v) -> S.ByteString -> v -> DBMS v ()
insertWith !f !key !val = do
  htvar <- access dbmTable
  liftSTM $ modifyTVar' htvar $ HMS.insertWith f key val
  update
{-# INLINE insertWith #-}

delete :: S.ByteString -> DBMS v ()
delete !key = do
  htvar <- access dbmTable
  liftSTM $ modifyTVar' htvar $ HMS.delete key
  update
{-# INLINE delete #-}

lookup :: S.ByteString -> DBMS v (Maybe v)
lookup !key = do
  htvar <- access dbmTable
  liftSTM $ HMS.lookup key <$> readTVar htvar
{-# INLINE lookup #-}

lookupDefault :: Default v => S.ByteString -> DBMS v v
lookupDefault !key = do
  htvar <- access dbmTable
  liftSTM $ fromMaybe def . HMS.lookup key <$> readTVar htvar
{-# INLINE lookupDefault #-}

keys :: Monad m => DBMS v (Source (DBMT v m) S.ByteString)
keys = do
  htvar <- access dbmTable
  ht <- liftSTM $ readTVar htvar
  return $ mapM_ yield $ HMS.keys ht
{-# INLINE keys #-}

update ::DBMS v ()
update = liftSTM =<< access dbmUpdate
{-# INLINE update #-}

transaction :: MonadIO m => DBMS v a -> DBMT v m a
transaction =
  lift . mapStateT (liftIO . atomically) . runIdentityT . unDBMT
{-# INLINE transaction #-}

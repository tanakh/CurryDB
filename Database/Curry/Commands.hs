{-# LANGUAGE BangPatterns #-}

module Database.Curry.Commands (
  -- Commands
  insert,
  insertWith,
  delete,
  lookup,
  lookupDefault,
  keys,

  -- Transaction
  transaction,
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Identity
import qualified Data.ByteString              as S
import           Data.Conduit
import           Data.Default
import           Data.Maybe

import qualified Database.Curry.HashMap       as HM
import           Database.Curry.Types

import           Prelude                      hiding (lookup)

insert :: S.ByteString -> v -> DBMS v ()
insert !key !val = do
  liftSTM . HM.insert key val =<< use dbmTable
  update
{-# INLINE insert #-}

insertWith :: (v -> v -> v) -> S.ByteString -> v -> DBMS v ()
insertWith !f !key !val = do
  liftSTM . HM.insertWith f key val =<< use dbmTable
  update
{-# INLINE insertWith #-}

delete :: S.ByteString -> DBMS v ()
delete !key = do
  liftSTM . HM.delete key =<< use dbmTable
  update
{-# INLINE delete #-}

lookup :: S.ByteString -> DBMS v (Maybe v)
lookup !key = do
  ht <- use dbmTable
  liftSTM $ HM.lookup key ht
{-# INLINE lookup #-}

lookupDefault :: Default v => S.ByteString -> DBMS v v
lookupDefault !key = fromMaybe def <$> lookup key
{-# INLINE lookupDefault #-}

keys :: Monad m => DBMS v (Source (DBMT v m) S.ByteString)
keys = do
  ht <- use dbmTable
  ks <- liftSTM $ HM.keys ht
  return $ mapM_ yield ks
{-# INLINE keys #-}

update ::DBMS v ()
update = liftSTM =<< use dbmUpdate
{-# INLINE update #-}

transaction :: MonadIO m => DBMS v a -> DBMT v m a
transaction =
  lift . mapStateT (liftIO . atomically) . runIdentityT . unDBMT
{-# INLINE transaction #-}

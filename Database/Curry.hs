{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Curry (
  -- DBM monad
  DBMT,
  runDBMT,

  -- DBM operations
  insert, insertWith,
  delete,
  lookup, lookupDefault,
  keys,

  transaction,
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Identity
import qualified Data.ByteString              as S
import           Data.Conduit
import           Data.Default
import qualified Data.HashMap.Strict          as HMS
import           Data.Lens
import           Data.Lens.Template
import           Data.Maybe
import           Data.Time

import           Prelude                      hiding (lookup)

type DBMT v m = DBMT_ (StateT (DBMState v) m)

newtype DBMT_ m a =
  DBMT_ { unDBMT :: IdentityT m a }
  deriving
    ( Functor, Applicative, Monad
    , MonadIO, MonadTrans, MonadBase b
    , MonadThrow, MonadResource
    )

deriving instance MonadState (DBMState v) m => MonadState (DBMState v) (DBMT_ m)

instance MonadTransControl DBMT_ where
  newtype StT DBMT_ a =
    StDBMT { unStDBM :: a }
  liftWith f =
    DBMT_ $ lift $ f $ liftM StDBMT . runIdentityT . unDBMT
  restoreT =
    DBMT_ . lift . liftM unStDBM

instance MonadBaseControl b m => MonadBaseControl b (DBMT_ m) where
  newtype StM (DBMT_ m) a = StMT { unStMT :: ComposeSt DBMT_ m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM     = defaultRestoreM   unStMT

data DBMState v
  = DBMState
    { _dbmTable      :: TVar (HMS.HashMap S.ByteString v)
    , _dbmUpdates    :: TVar Int
    , _dbmLastUpdate :: TVar UTCTime
    }

liftSTM :: STM a -> DBMT v STM a
liftSTM = lift . lift
{-# INLINE liftSTM #-}

initDBMState :: IO (DBMState v)
initDBMState =
  DBMState
    <$> newTVarIO HMS.empty
    <*> newTVarIO 0
    <*> (newTVarIO =<< getCurrentTime)

makeLens ''DBMState

runDBMT :: MonadIO m => DBMT v m a -> m a
runDBMT m = do
  st <- liftIO initDBMState
  evalStateT (runIdentityT $ unDBMT m) st

-----

insert :: S.ByteString -> v -> DBMT v STM ()
insert !key !val = do
  table <- access dbmTable
  liftSTM $ modifyTVar' table $ HMS.insert key val
{-# INLINE insert #-}

insertWith :: (v -> v -> v) -> S.ByteString -> v -> DBMT v STM ()
insertWith !f !key !val = do
  htvar <- access dbmTable
  liftSTM $ modifyTVar' htvar $ HMS.insertWith f key val
{-# INLINE insertWith #-}

delete :: S.ByteString -> DBMT v STM ()
delete !key = do
  htvar <- access dbmTable
  liftSTM $ modifyTVar' htvar $ HMS.delete key
{-# INLINE delete #-}

lookup :: S.ByteString -> DBMT v STM (Maybe v)
lookup !key = do
  htvar <- access dbmTable
  liftSTM $ HMS.lookup key <$> readTVar htvar
{-# INLINE lookup #-}

lookupDefault :: Default v => S.ByteString -> DBMT v STM v
lookupDefault !key = do
  htvar <- access dbmTable
  liftSTM $ fromMaybe def . HMS.lookup key <$> readTVar htvar
{-# INLINE lookupDefault #-}

keys :: Monad m => DBMT v STM (Source (DBMT v m) S.ByteString)
keys = do
  htvar <- access dbmTable
  ht <- liftSTM $ readTVar htvar
  return $ mapM_ yield $ HMS.keys ht
{-# INLINE keys #-}

transaction :: MonadIO m => DBMT v STM a -> DBMT v m a
transaction =
  DBMT_ . IdentityT . mapStateT (liftIO . atomically) . runIdentityT . unDBMT
{-# INLINE transaction #-}

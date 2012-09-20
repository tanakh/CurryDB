{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.KVS (
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
import qualified Data.ByteString.Char8        as S
import           Data.Conduit
import qualified Data.HashMap.Strict          as HMS
import           Data.Lens
import           Data.Lens.Template
import           Data.Time
import Data.Default
import Data.Maybe

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

initDBMState :: IO (DBMState v)
initDBMState = do
  DBMState
    <$> newTVarIO (HMS.empty)
    <*> newTVarIO 0
    <*> (newTVarIO =<< getCurrentTime)

makeLens ''DBMState

runDBMT :: MonadIO m => DBMT v m a -> m a
runDBMT m = do
  st <- liftIO initDBMState
  evalStateT (runIdentityT $ unDBMT m) st

insert :: (Functor m, MonadIO m) => S.ByteString -> v -> DBMT v m ()
insert key val = do
  table <- access dbmTable
  liftIO $ atomically $ modifyTVar' table $ HMS.insert key val
{-# INLINE insert #-}

insertWith :: (Functor m, MonadIO m)
              => (v -> v -> v)
              -> S.ByteString -> v -> DBMT v m ()
insertWith f key val = do
  htvar <- access dbmTable
  liftIO $ atomically $ modifyTVar' htvar $ HMS.insertWith f key val
{-# INLINE insertWith #-}

delete :: (Functor m, MonadIO m) => S.ByteString -> DBMT v m ()
delete key = do
  htvar <- access dbmTable
  liftIO $ atomically $ modifyTVar' htvar $ HMS.delete key
{-# INLINE delete #-}

lookup :: (Functor m, MonadIO m) => S.ByteString -> DBMT v m (Maybe v)
lookup key = do
  htvar <- access dbmTable
  liftIO $ HMS.lookup key <$> readTVarIO htvar
{-# INLINE lookup #-}

lookupDefault :: (Functor m, MonadIO m, Default v)
                 => S.ByteString -> DBMT v m v
lookupDefault key = do
  htvar <- access dbmTable
  liftIO $ fromMaybe def . HMS.lookup key <$> readTVarIO htvar
{-# INLINE lookupDefault #-}

keys :: MonadIO m => Source (DBMT v m) S.ByteString
keys = do
  htvar <- lift $ access dbmTable
  ht <- liftIO $ atomically $ readTVar htvar
  mapM_ yield $ HMS.keys ht

transaction :: DBMT v m a -> DBMT v m a
transaction = id -- FIXME:

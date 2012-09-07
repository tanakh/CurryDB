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
  lookup,
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

import           Prelude                      hiding (lookup)

type DBMT m = DBMT_ (StateT DBMState m)

newtype DBMT_ m a =
  DBMT_ { unDBMT :: IdentityT m a }
  deriving
    ( Functor, Applicative, Monad
    , MonadIO, MonadTrans, MonadBase b
    , MonadThrow, MonadResource
    )

deriving instance MonadState DBMState m => MonadState DBMState (DBMT_ m)

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

data DBMState
  = DBMState
    { _dbmTable      :: !(HMS.HashMap S.ByteString S.ByteString)
    , _dbmUpdates    :: TVar Int
    , _dbmLastUpdate :: TVar UTCTime
    }

initDBMState :: IO DBMState
initDBMState = do
  DBMState
    <$> pure HMS.empty
    <*> newTVarIO 0
    <*> (newTVarIO =<< getCurrentTime)

makeLens ''DBMState

runDBMT :: MonadIO m => DBMT m a -> m a
runDBMT m = do
  st <- liftIO initDBMState
  evalStateT (runIdentityT $ unDBMT m) st

insert :: (Functor m, Monad m) => S.ByteString -> S.ByteString -> DBMT m ()
insert key val = do
  void $ dbmTable !%= HMS.insert key val
{-# INLINE insert #-}

insertWith :: (Functor m, Monad m)
              => (S.ByteString -> S.ByteString -> S.ByteString)
              -> S.ByteString -> S.ByteString -> DBMT m ()
insertWith f key val = do
  void $ dbmTable !%= HMS.insertWith f key val
{-# INLINE insertWith #-}

delete :: (Functor m, Monad m) => S.ByteString -> DBMT m ()
delete key =
  void $ dbmTable !%= HMS.delete key
{-# INLINE delete #-}

lookup :: (Functor m, Monad m) => S.ByteString -> DBMT m (Maybe S.ByteString)
lookup key =
  HMS.lookup key <$> access dbmTable
{-# INLINE lookup #-}

keys :: Monad m => Source (DBMT m) S.ByteString
keys = do
  ht <- lift $ access dbmTable
  mapM_ yield $ HMS.keys ht

transaction :: DBMT m a -> DBMT m a
transaction = id -- FIXME:

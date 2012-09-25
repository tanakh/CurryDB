{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Curry (
  -- DBM monad
  DBMT,
  runDBMT,

  -- Configuration
  Config(..), def,
  SaveStrategy(..),

  -- DBM operations
  insert, insertWith,
  delete,
  lookup, lookupDefault,
  keys,

  transaction,
  ) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Logger
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
import qualified Filesystem.Path              as FP
import           Language.Haskell.TH.Syntax   (Loc (..))
import           System.IO
import           System.Log.FastLogger
import Control.Concurrent (threadDelay)

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

instance MonadIO m => MonadLogger (DBMT v m) where
  monadLoggerLog loc level msg = do
    logger <- gets _dbmLogger
    date <- liftIO $ loggerDate logger
    let (row, col) = loc_start loc
    liftIO $ loggerPutStr logger
      [ toLogStr date, LB " "
      , toLogStr (loc_module loc), LB ":"
      , LS (show row), LB ":", LS (show col), LB ": "
      , LB "[", LS (show level), LB "]: "
      , toLogStr msg
      , LB "\n"
      ]

data DBMState v
  = DBMState
    { _dbmTable      :: TVar (HMS.HashMap S.ByteString v)
    , _dbmUpdates    :: TVar Int
    , _dbmLastUpdate :: TVar UTCTime
    , _dbmLogger     :: Logger
    , _dbmConfig     :: Config
    }

data Config
  = Config
    { configPath         :: Maybe FP.FilePath
    , configSaveStrategy :: [SaveStrategy]
    , configVerbosity    :: LogLevel
    }

data SaveStrategy
  = SaveByFrequency
    { freqSecond  :: Int
    , freqUpdates :: Int
    }

makeLens ''DBMState

instance Default Config where
  def = Config
    { configPath = Nothing
    , configSaveStrategy = []
    , configVerbosity = LevelInfo
    }

liftSTM :: STM a -> DBMT v STM a
liftSTM = lift . lift
{-# INLINE liftSTM #-}

initDBMState :: Config -> IO (DBMState v)
initDBMState conf =
  DBMState
    <$> newTVarIO HMS.empty
    <*> newTVarIO 0
    <*> (newTVarIO =<< getCurrentTime)
    <*> mkLogger True stdout
    <*> pure conf

runDBMT :: (MonadIO m, MonadBaseControl IO m)
           => Config -> DBMT v m a -> m a
runDBMT conf m = do
  st <- liftIO $ initDBMState conf
  (`evalStateT` st) $ runIdentityT $ unDBMT $ control $ \run -> do
    _ <- async $ run saveThread
    run m

-----

saveThread :: MonadIO m => DBMT v m a
saveThread = forever $ ((liftIO . threadDelay $ 10^(6::Int)) >>) $ do
  -- $logInfo "polling"
  -- TODO: implement it
  return ()

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

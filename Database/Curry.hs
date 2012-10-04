{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
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

  -- Exec transaction
  transaction,
  ) where

import           Control.Applicative
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Exception            as E
import qualified Control.Exception.Lifted     as EL
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Logger
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Identity
import           Data.Binary
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import           Data.Conduit
import           Data.Default
import qualified Data.HashMap.Strict          as HMS
import           Data.Lens
import           Data.Lens.Template
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Time
import qualified Filesystem                   as FS
import qualified Filesystem.Path.CurrentOS    as FP
import           Language.Haskell.TH.Syntax   (Loc (..))
import           System.IO
import           System.Log.FastLogger

import           Database.Curry.Binary        ()

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
      , LB "[", LS (show level), LB "] "
      , toLogStr (loc_module loc), LB ":", LS (show row), LB ":", LS (show col), LB ": "
      , toLogStr msg
      , LB "\n"
      ]

data DBMState v
  = DBMState
    { _dbmTable  :: TVar (HMS.HashMap S.ByteString v)
    , _dbmUpdate :: STM ()
    , _dbmLogger :: Logger
    , _dbmConfig :: Config
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

initDBMState :: Config -> STM () -> IO (DBMState v)
initDBMState conf upd =
  DBMState
    <$> newTVarIO HMS.empty
    <*> pure upd
    <*> mkLogger True stdout
    <*> pure conf

runDBMT :: (MonadIO m, MonadBaseControl IO m, Binary v)
           => Config -> DBMT v m a -> m a
runDBMT conf m = do
  (upd, reset, saveReq) <- liftIO $ createNotifyer $ configSaveStrategy conf
  st <- liftIO $ initDBMState conf upd
  (`evalStateT` st) $ runIdentityT $ unDBMT $ do
    loadFromFile
    control $ \run -> do
      _ <- async $ run $ saveThread saveReq reset
      run (m `EL.finally` saveToFile)

-----

saveThread :: (Functor m, MonadIO m, Binary v)
              => TVar Bool -> STM () -> DBMT v m ()
saveThread saveReq reset = forever $ do
  liftIO $ atomically $ do
    req <- readTVar saveReq
    when (not req) retry
    writeTVar saveReq False
    reset
  saveToFile

createNotifyer :: [SaveStrategy] -> IO (STM (), STM (), TVar Bool)
createNotifyer strats = do
  timer   <- createTimer
  saveReq <- newTVarIO False

  let notify (SaveByFrequency {..})= do
        upd <- newTVarIO 0
        forkIO $ forever $ do
          start <- readTVarIO timer
          atomically $ do
            cur <- readTVar timer
            when (cur `diffUTCTime` start < fromIntegral freqSecond) retry
            num <- readTVar upd
            writeTVar upd 0
            when (num >= freqUpdates) $ writeTVar saveReq True
        return upd

  upds <- mapM notify strats
  let upd = mapM_ (\tv -> modifyTVar' tv (+1)) upds
      reset = mapM_ (\tv -> writeTVar tv 0) upds

  return (upd, reset, saveReq)

createTimer :: IO (TVar UTCTime)
createTimer = do
  curV <- newTVarIO =<< getCurrentTime
  _ <- async $ forever $ do
    threadDelay $ 10 ^ (6 :: Int)
    time <- getCurrentTime
    atomically $ writeTVar curV time
  return curV

saveToFile :: (MonadIO m, Binary v) => DBMT v m ()
saveToFile = do
  $logInfo "save to file..."
  Config {..} <- access dbmConfig
  let Just fname = configPath
  tbl <- liftIO . readTVarIO =<< access dbmTable
  err <- liftIO $ E.try $ atomicWriteFile fname tbl
  case err of
    Right _ ->
      return ()
    Left ioerr ->
      $logError $ "save error: " <> (T.pack $ show $ (ioerr :: IOError))

loadFromFile :: (MonadIO m, Binary v) => DBMT v m ()
loadFromFile = do
  Config {..} <- access dbmConfig
  case configPath of
    Nothing -> return ()
    Just path -> do
      $logInfo "load from file..."
      etbl <- liftIO $ E.try $ do
        v <- decode . L.fromChunks . (\x -> [x]) <$> FS.readFile path
        E.evaluate v
      case etbl of
        Right tbl -> do
          tv <- access dbmTable
          liftIO $ atomically $ writeTVar tv tbl
        Left err -> do
          $logInfo $ "fail to load " <> ": " <> (T.pack $ show (err :: E.SomeException))

-- FIXME: move to utils
atomicWriteFile :: Binary b => FP.FilePath -> b -> IO ()
atomicWriteFile path b = do
  let tmpPath = path FP.<.> "tmp"
  FS.withFile tmpPath WriteMode $ \h ->
    L.hPut h $ encode b
  -- rename is atomic
  FS.rename tmpPath path

-----

insert :: S.ByteString -> v -> DBMT v STM ()
insert !key !val = do
  table <- access dbmTable
  liftSTM $ modifyTVar' table $ HMS.insert key val
  update
{-# INLINE insert #-}

insertWith :: (v -> v -> v) -> S.ByteString -> v -> DBMT v STM ()
insertWith !f !key !val = do
  htvar <- access dbmTable
  liftSTM $ modifyTVar' htvar $ HMS.insertWith f key val
  update
{-# INLINE insertWith #-}

delete :: S.ByteString -> DBMT v STM ()
delete !key = do
  htvar <- access dbmTable
  liftSTM $ modifyTVar' htvar $ HMS.delete key
  update
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

update ::DBMT v STM ()
update = liftSTM =<< access dbmUpdate
{-# INLINE update #-}

transaction :: MonadIO m => DBMT v STM a -> DBMT v m a
transaction =
  DBMT_ . IdentityT . mapStateT (liftIO . atomically) . runIdentityT . unDBMT
{-# INLINE transaction #-}

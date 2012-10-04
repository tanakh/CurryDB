{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Curry (
  -- run DBM Monad
  runDBMT,

  module Database.Curry.Commands,
  module Database.Curry.Types,
  ) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted     as EL
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Identity
import           Data.Binary
import           Data.Default
import qualified Data.HashMap.Strict          as HMS
import           System.IO
import           System.Log.FastLogger

import           Database.Curry.Binary        ()
import           Database.Curry.Commands
import           Database.Curry.Storage
import           Database.Curry.Types

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

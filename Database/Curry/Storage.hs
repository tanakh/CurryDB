{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Curry.Storage (
  saveThread,
  createNotifyer,

  saveToFile,
  loadFromFile,
  ) where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Exception          as E
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.State.Strict
import           Data.Binary
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Time

import           Database.Curry.HashMap     as HM
import           Database.Curry.Types

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
        _ <- forkIO $ forever $ do
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
  Config {..} <- use dbmConfig
  case configPath of
    Nothing -> return ()
    Just path -> do
      $logInfo "save to file..."
      err <- liftIO . E.try . HM.save path =<< use dbmTable
      case err of
        Right _ ->
          return ()
        Left ioerr ->
          $logError $ "save error: " <> (T.pack $ show $ (ioerr :: IOError))

loadFromFile :: (MonadIO m, Binary v) => DBMT v m ()
loadFromFile = do
  Config {..} <- use dbmConfig
  case configPath of
    Nothing -> return ()
    Just path -> do
      $logInfo "load from file..."
      tbl <- use dbmTable
      res <- liftIO . E.try . HM.load path =<< use dbmTable
      case res of
        Right () -> return ()
        Left err -> do
          $logInfo $ "fail to load " <> ": " <> (T.pack $ show (err :: E.SomeException))

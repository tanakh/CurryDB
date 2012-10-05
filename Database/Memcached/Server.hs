module Database.Memcached.Server (
  runServer,

  ServerSettings,
  serverSettings,
  ) where

import           Blaze.ByteString.Builder
import           Control.Monad.Trans
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import           Network                  (withSocketsDo)

import           Database.Curry
import           Database.Memcached.Commands

runServer :: ServerSettings (MemcachedT IO) -> IO ()
runServer ss = withSocketsDo $ runDBMT def $ runTCPServer ss server

server :: Application (MemcachedT IO)
server ss =
  appSource ss $$ conduitParser parseCommand =$ awaitForever p =$ appSink ss
  where
    p (_range, req) = do
      liftIO $ putStrLn $ "server: " ++ show req
      resp <- lift $ execCommand req
      yield . toByteString . fromResponse $ resp

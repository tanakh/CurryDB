module Database.Memcached.Server (
  runServer,
  ServerSettings(..),
  ) where

import           Blaze.ByteString.Builder
import           Control.Monad.Trans
import qualified Data.ByteString          as S
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import           Network                  (withSocketsDo)

import           Database.Curry
import           Database.Memcached.Commands

runServer :: ServerSettings -> IO ()
runServer ss = withSocketsDo $ runDBMT $ runTCPServer ss server

server :: Application (DBMT S.ByteString IO)
server src sink =
  src $$ conduitParser parseCommand =$ awaitForever p =$ sink
  where
    p (_range, req) = do
      liftIO $ putStrLn $ "server: " ++ show req
      resp <- lift $ execCommand req
      yield . toByteString . fromResponse $ resp

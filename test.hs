{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Blaze.ByteString.Builder
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Trans
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import           Network                  (withSocketsDo)
import           Prelude                  hiding (lookup)

import           Database.KVS             as KVS
import           Database.Memcached

main :: IO ()
main = server `race_` client

server :: IO ()
server = withSocketsDo $ runDBMT $ do
  runTCPServer (ServerSettings 3333 "*") app

app :: Application (DBMT IO)
app src sink =
  src $$ conduitParser parseCommand =$ awaitForever p =$ sink
  where
    p (_range, req) = do
      liftIO $ putStrLn $ "server: " ++ show req
      resp <- lift $ execCommand req
      yield . toByteString . fromResponse $ resp

client :: IO ()
client = withSocketsDo $ do
  runTCPClient (ClientSettings 3333 "localhost") $ \src sink ->
    src $$ cond =$ sink
  where
    cond = do
      resp1 <- yield "set foo 0 0 3\r\nbar\r\n" >> await
      liftIO $ putStrLn $ "client : " ++ show resp1

      resp2 <- yield "add foo 0 0 3\r\nbaz\r\n" >> await
      liftIO $ putStrLn $ "client : " ++ show resp2

      resp3 <- yield "get foo hoge\r\n" >> await
      liftIO $ putStrLn $ "client : " ++ show resp3

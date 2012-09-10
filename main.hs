{-# LANGUAGE OverloadedStrings #-}

import           Blaze.ByteString.Builder
import           Control.Monad.Trans
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import           Network                  (withSocketsDo)
import           Prelude                  hiding (lookup)

import           Database.KVS             as KVS
import           Database.Memcached

main :: IO ()
main = server

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

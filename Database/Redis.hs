{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Redis (
  runServer,
  ServerSettings,
  ) where

import           Blaze.ByteString.Builder
import           Data.Conduit
import           Data.Conduit.Attoparsec  (conduitParser)
import           Data.Conduit.Internal    (sinkToPipe, sourceToPipe)
import           Data.Conduit.Network     (ServerSettings (..), runTCPServer)
import           Network                  (withSocketsDo)

import           Database.Curry
import           Database.Redis.Builder
import           Database.Redis.Commands
import           Database.Redis.Parser

runServer :: ServerSettings -> IO ()
runServer ss = withSocketsDo $ runDBMT $ runTCPServer ss $ \src sink -> do
  runPipe
    $   sourceToPipe src
    >+> injectLeftovers (conduitParser parseRequest)
    >+> mapOutput (toByteString . fromReply) process
    >+> sinkToPipe sink

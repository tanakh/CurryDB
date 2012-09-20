{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Redis (
  runServer,
  ) where

import           Data.Conduit
import           Data.Conduit.Attoparsec (conduitParser)
import           Data.Conduit.Blaze      (builderToByteStringFlush)
import           Data.Conduit.Internal   (sinkToPipe, sourceToPipe)
import qualified Data.Conduit.List       as CL
import           Data.Conduit.Network    (ServerSettings, runTCPServer)
import           Network                 (withSocketsDo)

import           Database.Curry
import           Database.Redis.Builder
import           Database.Redis.Commands
import           Database.Redis.Parser

runServer :: ServerSettings -> IO ()
runServer ss = withSocketsDo $ runDBMT $ runTCPServer ss $ \src sink -> do
  runPipe
    $   sourceToPipe src
    >+> injectLeftovers (conduitParser parseRequest)
    >+> process
    >+> CL.map fromReply
    >+> CL.concatMap (\bld -> [Chunk bld, Flush])
    >+> mapOutputMaybe unChunk builderToByteStringFlush
    >+> sinkToPipe sink
  where
    unChunk Flush = Nothing
    unChunk (Chunk s) = Just s

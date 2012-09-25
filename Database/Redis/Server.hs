{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Redis.Server (
  runServer,
  ServerSettings,
  ) where

import           Blaze.ByteString.Builder
-- import           Control.Monad.Logger
import           Data.Conduit
import           Data.Conduit.Attoparsec  (conduitParser)
import           Data.Conduit.Internal    (sinkToPipe, sourceToPipe)
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Network     (ServerSettings (..), runTCPServer)
-- import qualified Data.Text                as T
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
    -- >+> CL.mapM (\req -> $logInfo (T.pack $ show $ snd req) >> return req)
    >+> CL.mapM (fmap (toByteString . fromReply) . process)
    >+> sinkToPipe sink

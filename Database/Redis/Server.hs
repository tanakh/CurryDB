{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Redis.Server (
  runServer,
  ServerSettings, serverSettings,
  def,
  ) where

import           Blaze.ByteString.Builder
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Conduit.Attoparsec  (conduitParser)
import           Data.Conduit.Internal    (sinkToPipe, sourceToPipe)
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Network
import           Data.Monoid
import qualified Data.Text                as T
import           Network                  (withSocketsDo)

import           Database.Curry
import           Database.Redis.Builder
import           Database.Redis.Commands
import           Database.Redis.Parser
import           Database.Redis.Types

runServer :: Config -> ServerSettings (RedisT IO) -> IO ()
runServer conf ss = withSocketsDo $ runDBMT conf $ do
  $logInfo $ "listen on port " <> (T.pack $ show $ serverPort ss) <> "."
  runTCPServer ss $ \ad -> do
    runPipe
      $   sourceToPipe (appSource ad)
      >+> injectLeftovers (conduitParser parseRequest)
      -- >+> CL.mapM (\req -> $logInfo (T.pack $ show $ snd req) >> return req)
      >+> CL.mapM (fmap (toByteString . fromReply) . process)
      >+> sinkToPipe (appSink ad)

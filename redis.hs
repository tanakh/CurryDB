{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit.Network
import Database.Redis
import System.Remote.Monitoring

main :: IO ()
main = do
  forkServer "localhost" 8000
  runServer $ ServerSettings 3335 "*"

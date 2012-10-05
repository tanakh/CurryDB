{-# LANGUAGE OverloadedStrings #-}

import Database.Curry
import Database.Redis
import System.Remote.Monitoring (forkServer)

config :: Config
config = def
  { configPath = Just "dump.cdb"
  , configSaveStrategy =
    [ SaveByFrequency 900 1
    , SaveByFrequency 300 10
    , SaveByFrequency 60  10000
    ]
  }

main :: IO ()
main = do
  forkServer "localhost" 8000
  runServer config $ serverSettings 8854 "*"

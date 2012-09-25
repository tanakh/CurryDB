{-# LANGUAGE OverloadedStrings #-}

import Database.Redis
import System.Remote.Monitoring (forkServer)

main :: IO ()
main = do
  forkServer "localhost" 8000
  runServer def $ ServerSettings 8854 "*"

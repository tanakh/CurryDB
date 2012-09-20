{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit.Network
import Database.Redis

main :: IO ()
main = runServer $ ServerSettings 3334 "*"

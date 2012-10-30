{-# LANGUAGE OverloadedStrings #-}

import           Database.Memcached.Server

main :: IO ()
main = runServer (serverSettings 3334 "*")

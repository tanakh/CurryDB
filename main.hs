{-# LANGUAGE OverloadedStrings #-}

import           Database.Memcached.Server

main :: IO ()
main = runServer $ ServerSettings 3333 "*"

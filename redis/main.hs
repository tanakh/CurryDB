{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Database.Curry
import           Database.Redis
import           Options.Applicative
-- import           System.Remote.Monitoring (forkServer)
import           Filesystem.Path.CurrentOS (decodeString)

data Option = Option
  { optPort :: Int
  , optPath :: Maybe FilePath
  }

opts :: Parser Option
opts = Option
  <$> option
    ( long "port" & short 'p' & metavar "PORT"
    & value 8854
    & help "Port Number"
    )
  <*> option
    ( long "file" & short 'f' & metavar "FILE"
    & reader (Just . Just)
    & value Nothing
    & help "dump file"
    )

main :: IO ()
main = execParser optsInfo >>= \Option{..} -> do
  -- forkServer "localhost" 8000 -- monitoring
  (`runServer` serverSettings optPort "*") $ def
    { configPath = fmap decodeString optPath
    , configSaveStrategy =
      [ SaveByFrequency 900 1
      , SaveByFrequency 300 10
      , SaveByFrequency 60  10000
      ]
    }
  where
    optsInfo = info (helper <*> opts)
      ( fullDesc
      & header "curry-redis - a redis clone over CurryDB"
      )

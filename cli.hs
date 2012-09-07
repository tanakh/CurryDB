import System.Console.Haskeline
import Data.Conduit
import Data.Conduit.Network
import Network (withSocketsDo)
import Control.Monad.Trans
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as S

main :: IO ()
main = withSocketsDo $ do
  req <- newEmptyTMVarIO
  res <- newEmptyTMVarIO
  race_
    (runInputT defaultSettings $ repl req res)
    (runTCPClient (ClientSettings 3333 "localhost")
     (\src sink -> src $$ cond req res =$ sink))

repl :: MonadException m => TMVar String -> TMVar String -> InputT m ()
repl req res = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> return ()
    Just "quit" -> return ()
    Just input -> do
      liftIO $ atomically $ putTMVar req input
      r <- liftIO $ atomically $ takeTMVar res
      outputStrLn $ "Input was: " ++ r
      repl req res

cond :: MonadIO m => TMVar String -> TMVar String -> Pipe l S.ByteString S.ByteString u m ()
cond req res = do
  r <- liftIO $ atomically $ takeTMVar req
  liftIO $ print r
  yield $ S.pack $ r ++ "\r\n"
  mb <- await
  case mb of
    Nothing -> return ()
    Just bs -> do
      liftIO $ atomically $ putTMVar res $ S.unpack bs
      cond req res

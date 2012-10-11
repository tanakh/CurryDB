
main :: IO ()
main = return ()

{-
import Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8   as S
import           Prelude                 hiding (lookup)
import Data.Char
import System.Random.Mersenne.Pure64
import System.IO.Unsafe
import Data.IORef

ior :: IORef PureMT
ior = unsafePerformIO $ newIORef =<< newPureMT
{-# NOINLINE ior #-}

randomString :: Int -> PureMT -> (String, PureMT)
randomString n m = go 0 "" m where
  go i ss mt
    | i == n = (ss, mt)
    | otherwise =
      let (x, nt) = randomInt mt in
      go (i+1) (f x :ss) nt
  f x = chr (ord 'a' + x `mod` 26)

main :: IO ()
main = runDBMT $ do
  replicateM_ 1000000 $ do
    (key, val) <- liftIO $ do
      mt1 <- readIORef ior
      let (key, mt2) = randomString 8 mt1
      let (val, mt3) = randomString 8 mt2
      writeIORef ior mt3
      return (key, val)
    insert (S.pack key) (S.pack val)
-}

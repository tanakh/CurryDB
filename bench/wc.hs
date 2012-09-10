import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import           Prelude               hiding (lookup)

import           Database.KVS

bread :: S.ByteString -> Int
bread = maybe 0 fst . S.readInt
{-# INLINE bread #-}

bshow :: Int -> S.ByteString
bshow = S.pack . show
{-# INLINE bshow #-}

main :: IO ()
main = runDBMT $ do
  -- word count
  ws <- liftIO $ S.words <$> S.readFile "english.50MB"
  forM_ ws $ \word -> do
    cnt <- maybe 0 bread <$> lookup word
    insert word $ bshow (cnt + 1 :: Int)

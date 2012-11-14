{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent.Chan.Lifted
import           Control.Concurrent.Lifted
import           Control.Monad
import           Control.Monad.Trans
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.List                      (sort)

import           Test.Hspec
-- import Test.QuickCheck
-- import Test.HUnit

import           Database.Curry

import           Prelude                        hiding (lookup)

main :: IO ()
main = hspec $ do
  describe "core apis" $ do
    it "insert correctly" $ do
      runDBMT def $ do
        v1 <- transaction $ lookup "foo"
        liftIO $ v1 `shouldBe` Nothing
        transaction $ insert "foo" (123 :: Int)
        v2 <- transaction $ lookup "foo"
        liftIO $ v2 `shouldBe` Just 123
        v3 <- transaction $ lookup "bar"
        liftIO $ v3 `shouldBe` Nothing
      return () :: IO ()

    it "insertWith correctly" $ do
      runDBMT def $ do
        transaction $ insert     "foo" (123 :: Int)
        transaction $ insertWith (+) "foo" 456
        v1 <- transaction $ lookup "foo"
        liftIO $ v1 `shouldBe` Just (123 + 456)
      return () :: IO ()

    it "delete correctly" $ do
      runDBMT def $ do
        transaction $ insert     "foo" (123 :: Int)
        v1 <- transaction $ lookup "foo"
        liftIO $ v1 `shouldBe` Just 123
        transaction $ delete     "foo"
        v2 <- transaction $ lookup "foo"
        liftIO $ v2 `shouldBe` Nothing
      return () :: IO ()

    it "lookupDefault correctly" $ do
      runDBMT def $ do
        v1 <- transaction $ lookupDefault "foo"
        liftIO $ v1 `shouldBe` 0
        transaction $ insert     "foo" (123 :: Int)
        v2 <- transaction $ lookupDefault "foo"
        liftIO $ v2 `shouldBe` 123
      return () :: IO ()

    it "keys correctly" $ do
      runDBMT def $ do
        transaction $ insert "foo" (123 :: Int)
        transaction $ insert "bar" (456 :: Int)
        transaction $ insert "baz" (789 :: Int)
        ks <- transaction $ keys
        ls <- ks $$ CL.consume
        liftIO $ sort ls `shouldBe` sort ["foo", "bar", "baz"]
      return () :: IO ()

    it "keys correctly even after updates" $ do
      runDBMT def $ do
        transaction $ insert "foo" (123 :: Int)
        transaction $ insert "bar" (456 :: Int)
        transaction $ insert "baz" (789 :: Int)
        ks <- transaction $ keys
        transaction $ insert "hoge" (123 :: Int)
        transaction $ insert "moge" (456 :: Int)
        ls <- ks $$ CL.consume
        liftIO $ sort ls `shouldBe` sort ["foo", "bar", "baz"]
      return () :: IO ()

    it "transaction correctly" $ do
      runDBMT def $ do
        transaction $ insert "foo" (123 :: Int)
        transaction $ insert "bar" (456 :: Int)
        transaction $ do
          insertWith (+) "foo" 456
          insertWith subtract "bar" 123
        v1 <- transaction $ lookup "foo"
        liftIO $ v1 `shouldBe` Just (123 + 456)
        v2 <- transaction $ lookup "bar"
        liftIO $ v2 `shouldBe` Just (456 - 123)
      return () :: IO ()

    it "transaction rollbacks correctly" $ do
      runDBMT def $ do
        transaction $ insert "foo" (123 :: Int)
        transaction $ insert "bar" (456 :: Int)
        transaction $ do
          insertWith (+) "foo" 456
          insertWith subtract "bar" 123
          insert "baz" 789
          lift mzero
          <|> return ()
        v1 <- transaction $ lookup "foo"
        liftIO $ v1 `shouldBe` Just 123
        v2 <- transaction $ lookup "bar"
        liftIO $ v2 `shouldBe` Just 456
        v3 <- transaction $ lookup "baz"
        liftIO $ v3 `shouldBe` Nothing
      return () :: IO ()

  describe "concurrent support" $ do
    it "does not race" $ do
      runDBMT def $ do
        chan <- newChan
        replicateM_ 10 $ fork $ do
          replicateM_ 10000 $ do
            transaction $ insertWith (+) "foo" (1 :: Int)
          writeChan chan ()
        replicateM_ 10 $ readChan chan
        cnt <- transaction $ lookupDefault "foo"
        liftIO $ cnt `shouldBe` 100000
      return () :: IO ()

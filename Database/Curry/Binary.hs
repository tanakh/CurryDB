{-# LANGUAGE FlexibleInstances #-}

module Database.Curry.Binary where

import           Control.Applicative
import           Data.Binary
import qualified Data.ByteString     as S
import qualified Data.HashMap.Strict as HMS

instance Binary v => Binary (HMS.HashMap S.ByteString v) where
  put = put . HMS.toList
  get = HMS.fromList <$> get

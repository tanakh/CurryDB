{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Builder (
  fromReply,
  ) where

import           Blaze.ByteString.Builder
import           Blaze.Text
import qualified Data.ByteString          as S
import           Data.Monoid

import           Database.Redis.Types

fromReply :: Reply -> Builder
fromReply rep = case rep of
  StatusReply stat ->
    fromByteString "+" <> fromByteString stat <> crlf
  ErrorReply err ->
    fromByteString "-" <> fromByteString err <> crlf
  IntReply n ->
    fromByteString ":" <> integral n <> crlf
  BulkReply mb ->
    fromBulkReply mb
  MultiBulkReply Nothing ->
    fromByteString "*-1" <> crlf
  MultiBulkReply (Just bss) ->
    fromByteString "*" <> integral (length bss) <> crlf <>
    mconcat (map fromBulkReply bss)
{-# INLINE fromReply #-}

fromBulkReply :: Maybe S.ByteString -> Builder
fromBulkReply Nothing =
  fromByteString "$-1" <> crlf
fromBulkReply (Just bs) =
  fromByteString "$" <> integral (S.length bs) <> crlf <>
  fromByteString bs <> crlf
{-# INLINE fromBulkReply #-}

crlf :: Builder
crlf = fromByteString "\r\n"
{-# INLINE crlf #-}

{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Builder (
  fromReply,
  ) where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import qualified Data.ByteString                as S
import           Data.Monoid

import           Database.Redis.Types

fromReply :: Reply -> Builder
fromReply rep = case rep of
  StatusReply stat ->
    fromByteString "+" <> fromByteString stat <> crlf
  ErrorReply err ->
    fromByteString "-" <> fromByteString err <> crlf
  IntReply n ->
    fromByteString ":" <> fromString (show n) <> crlf
  BulkReply mb ->
    fromBulkReply mb
  MultiBulkReply Nothing ->
    fromByteString "*-1" <> crlf
  MultiBulkReply (Just bss) ->
    fromByteString "*" <> fromString (show $ length bss) <> crlf <>
    mconcat [ fromBulkReply bs | bs <- bss ]
  where
    fromBulkReply Nothing =
      fromByteString "$-1" <> crlf
    fromBulkReply (Just bs) =
      fromByteString "$" <> fromString (show $ S.length bs) <> crlf <>
      fromByteString bs <> crlf

    crlf = fromByteString "\r\n"
{-# INLINE fromReply #-}

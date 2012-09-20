{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Parser (
  parseRequest,
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Char8 as A

import           Database.Redis.Types

parseRequest :: Parser Request
parseRequest = unified <|> inline where
  unified = do
    n <- char '*' *> decimal <* crlf
    Request <$> replicateM n arg

  arg = do
    n <- char '$' *> decimal <* crlf
    A.take n <* crlf

  inline = do
    cmd <- A.takeWhile1 A.isAlpha_ascii
    Request <$> ((cmd:) <$> many (char ' ' *> A.takeTill isSpace)) <* crlf

  crlf = string "\r\n"

{-# INLINE parseRequest #-}

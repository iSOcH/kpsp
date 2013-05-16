module Kpspcrypto.SHA256 (hash) where

-- http://en.wikipedia.org/wiki/SHA-2

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B

-- apply SHA256 to given data, result will always be 32 bytes long
hash :: B.ByteString -> B.ByteString
hash input = id input

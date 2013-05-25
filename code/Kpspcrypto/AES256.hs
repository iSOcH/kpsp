module Kpspcrypto.AES256 (encode, decode, blocklen, keylen) where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B

type Block = B.ByteString
type Key = B.ByteString

{-----------------
encoding functions
-----------------}
-- crypt a single block using ECB and the supplied key
encode :: Key -> Block -> Block
encode key plain = id plain




{-----------------
decoding functions
-----------------}
decode :: Key -> Block -> Block
decode key crypted = id crypted


{---------
properties
---------}
blocklen :: Int
blocklen = 16

keylen :: Int
keylen = 32

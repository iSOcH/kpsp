module Kpspcrypto.AES256 (encode, decode) where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Codec.Encryption.AES as CEAES
import Data.Word
import Data.LargeWord

import Kpspcrypto.Serial

type Block = B.ByteString
type Key = B.ByteString

{-----------------
encoding functions
-----------------}
-- crypt a single block using ECB and the supplied key
encode :: Key -> Block -> Block
encode key plain = w1282b $ CEAES.encrypt keyw plainw
	where
		keyw = b2w128 key
		plainw = b2w128 plain

{-----------------
decoding functions
-----------------}
decode :: Key -> Block -> Block
decode key crypted = w1282b $ CEAES.decrypt keyw cryptedw
	where
		keyw = b2w128 key
		cryptedw = b2w128 crypted


{------
helpers
------}
-- converts the last 16 Bytes of a BString to a Word128
b2w128 :: B.ByteString -> Word128
b2w128 = fromIntegral . asInt

-- converts a Word128 to a 16 Byte BString
w1282b :: Word128 -> B.ByteString
w1282b s = B.replicate (16-clen) '\0' `B.append` converted
	where
		converted = asStr $ toInteger s
		clen = B.length converted

{----
tests
----}


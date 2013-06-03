module Kpspcrypto.AES256 (encode, decode) where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

{--
this module uses Codec.Encryption.AES from the "crypto"-Package
to perform the actual encryption and decryption functions
the Word128-Interface of Codec.Encryption.AES is converted to
a simpler to use Interface using ByteStrings
--}

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
-- tests whether a given string (first in tuple) which gets
-- encrypted and then decrypted using the same or different
-- key(s) is (not) the same as the original string

runTests :: Bool
runTests = and [testAES test | test <- tests]

-- runs tests from "tests"
-- compares d(e(plain)) and plain using the supplied eq-function
-- see comment of "tests" for further information
testAES :: (Block,(Block->Block->Bool),(Block->Block),(Block->Block)) -> Bool
testAES (plain,eq,e,d) = plain `eq` (d $ e plain)

-- different keys
key1 = "justAKeyjustBKey" :: B.ByteString
key2 = "justBKeyjustAKey" :: B.ByteString

-- partially apply the encode and decode functions using a key
-- results in functions of the type (Block -> Block)
e1 = encode key1
e2 = encode key2
d1 = decode key1
d2 = decode key2

-- (plaintext, equality-function, encoding function, decoding function)
-- the equality-function should return true if d(e(plain)) matches
-- the expected result, if you decode using another key than the one
-- used for encode, you expect the result to be different from plain,
-- thus you need to supply (/=) as "equality"-function
tests :: [(Block,(Block->Block->Bool),(Block->Block),(Block->Block))]
tests = [(nulls,(==),e1,d1)
		,(nulls,(/=),e1,d2)
		,(nulls,(/=),e2,d1)
		,(nulls,(==),e2,d2)
		,(as,(==),e1,d1)
		,(as,(/=),e1,d2)
		,(as,(/=),e2,d1)
		,(as,(==),e2,d2)
		]
	where
		nulls = B.replicate 16 '\0'
		as = B.replicate 16 'a'

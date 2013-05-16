module Kpspcrypto.RSA (encrypt, sign, decrypt, checksig) where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as DB
import qualified Kpspcrypto.Base64 as B64

-- first part is e or d, second is n
type Pubkey = (B.ByteString, B.ByteString)
type Privkey = (B.ByteString, B.ByteString)


{---------------
public functions
---------------}
encrypt :: Pubkey -> B.ByteString -> B.ByteString
encrypt (eIn, nIn) msgIn = toStr $ modexp msg e n
	where
		e = toInt eIn
		n = toInt nIn
		msg = toInt msgIn

sign :: Privkey -> B.ByteString -> B.ByteString
sign = encrypt

decrypt :: Privkey -> B.ByteString -> B.ByteString
decrypt = encrypt

checksig :: Pubkey -> B.ByteString -> B.ByteString -> Bool
checksig pubkey sig msg = encrypt pubkey sig == msg

{---------------
helper functions
---------------}
-- modexp b e n returns b^e mod n
-- slightly modified from http://pastebin.com/m142c0ca
modexp :: Integer -> Integer -> Integer -> Integer
modexp b 0 n = 1
modexp b e n
	| even e = (modexp b (e `div` 2) n) ^ 2 `mod` n
	| otherwise = (b * modexp b (e-1) n) `mod` n

{-----------------------------
less related utility functions
-----------------------------}
-- from http://stackoverflow.com/questions/7815402/convert-a-lazy-bytestring-to-a-strict-bytestring
toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

toLazy :: B.ByteString -> BL.ByteString
toLazy strict = BL.fromChunks [strict]

-- converts an Integer to Base64 encoded ByteString
toStr :: Integer -> B.ByteString
toStr = B64.encode . toStrict . DB.encode

-- reads a Base64 encoded Integer from a ByteString
toInt :: B.ByteString -> Integer
toInt = DB.decode . toLazy . B64.decode

{---------------------------------------------
sample data (from
http://de.wikipedia.org/wiki/RSA-Kryptosystem)
---------------------------------------------}
exmsg = toStr 7

expub = (toStr 23, toStr 143)
expriv = (toStr 47, toStr 143)

expub2 = (toStr 1721, toStr 263713)
expriv2 = (toStr 1373, toStr 263713)

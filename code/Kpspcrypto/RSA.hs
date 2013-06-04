module Kpspcrypto.RSA (encrypt, sign, decrypt, checksig) where

-- Main RSA module. Functions to en- and decrypt with RSA can be found here. 

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Kpspcrypto.Base64 as B64
import Kpspcrypto.Serial

type KeyFileContent = B.ByteString
-- first part is e or d, second is n
type Pubkey = (B.ByteString, B.ByteString)
type Privkey = Pubkey


{---------------
public functions
---------------}
-- encrypt the content of a plain text with the supplied key (keyfilecontent)
encrypt :: KeyFileContent -> B.ByteString -> B.ByteString
encrypt key msgIn = asStr $ modexp msg e n
	where
		e = toInt eIn
		n = toInt nIn
		msg = asInt msgIn
		(eIn,nIn) = fromFile key

-- function to sign -> hash
sign :: KeyFileContent -> B.ByteString -> B.ByteString
sign = encrypt


decrypt :: KeyFileContent -> B.ByteString -> B.ByteString
decrypt = encrypt

checksig :: KeyFileContent -> B.ByteString -> B.ByteString -> Bool
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
-- converts an Integer to Base64 encoded ByteString
toStr :: Integer -> B.ByteString
toStr = B64.encode . asStr

-- reads a Base64 encoded Integer from a ByteString
toInt :: B.ByteString -> Integer
toInt = asInt . B64.decode

-- extracts key from file
fromFile :: B.ByteString -> Pubkey
fromFile file = (e,n)
	where
		contentline = B.lines file !! 1
		[e,n] = B.split ',' contentline

{---------------------------------------------
sample data (from
http://de.wikipedia.org/wiki/RSA-Kryptosystem)
---------------------------------------------}
rsapubkey = "----BEGIN RSA PUBLIC KEY----\nFw==,jw==\n----END RSA PUBLIC KEY----" :: B.ByteString
rsaprivkey = "----BEGIN RSA PRIVATE KEY----\nLw==,jw==\n----END RSA PRIVATE KEY----" :: B.ByteString
rsarecvpubkey = "----BEGIN RSA PUBLIC KEY----\nBrk=,BAYh\n----END RSA PUBLIC KEY----" :: B.ByteString
rsarecvprivkey = "----BEGIN RSA PRIVATE KEY----\nBV0=,BAYh\n----END RSA PRIVATE KEY----" :: B.ByteString

exmsg = toStr 7

expub = (toStr 23, toStr 143)
expriv = (toStr 47, toStr 143)

expub2 = (toStr 1721, toStr 263713)
expriv2 = (toStr 1373, toStr 263713)

smalltest = [checksig rsapubkey (sign rsaprivkey str) str | str <- map B.singleton ['\0'..'\140']]
smalltest2 = [checksig rsarecvpubkey (sign rsarecvprivkey str) str | str <- map B.singleton ['\0'..'\255']]

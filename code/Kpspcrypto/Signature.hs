module Kpspcrypto.Signature where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B

import qualified Kpspcrypto.Base64 as B64
import qualified Kpspcrypto.RSA as RSA
import qualified Kpspcrypto.SHA256 as SHA
import Kpspcrypto.Pad
import Kpspcrypto.Msg

genMsgPart :: AsymCipher -> AsymKey -> HashType -> [MsgPart] -> MsgPart
genMsgPart "RSA" akey "SHA256" [kcpart,msgcpart] = MsgPart SIGNATURE ["RSA","SHA256"] signature
	where
		hashed = SHA.hash $ B.concat [content kcpart, content msgcpart]
		signed = map B64.encode [RSA.sign akey block | block <- pad 1 hashed] --TODO: groessere Keys, dann auf 8 Byte oder so anwenden
		signature = B64.encode $ B.intercalate "," signed

rsapubkey = "----BEGIN RSA PUBLIC KEY----\nFw==,jw==\n----END RSA PUBLIC KEY----" :: B.ByteString
rsaprivkey = "----BEGIN RSA PRIVATE KEY----\nLw==,jw==\n----END RSA PRIVATE KEY----" :: B.ByteString
rsarecvpubkey = "----BEGIN RSA PUBLIC KEY----\nBrk=,BAYh\n----END RSA PUBLIC KEY----" :: B.ByteString
rsarecvprivkey = "----BEGIN RSA PRIVATE KEY----\nBV0=,BAYh\n----END RSA PRIVATE KEY----" :: B.ByteString

module Kpspcrypto.Signature where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.List
import Data.Maybe

import qualified Kpspcrypto.Base64 as B64
import qualified Kpspcrypto.RSA as RSA
import qualified Kpspcrypto.SHA256 as SHA
import Kpspcrypto.Pad
import Kpspcrypto.Msg 

-- create a signature msgpart which contains the signed hash of the other msgparts
genMsgPart :: AsymCipher -> AsymKey -> HashType -> [MsgPart] -> MsgPart
genMsgPart "RSA" akey "SHA256" [kcpart,msgcpart] = MsgPart SIGNATURE ["RSA","SHA256"] signature
	where
		hashed = SHA.hash $ B.concat [content kcpart, content msgcpart]
		signed = map B64.encode [RSA.sign akey blocks | blocks <- block 6 hashed]
		signature = B.intercalate "," signed
		

-- verifies the signature of the whole msg
verifySig :: AsymKey -> [MsgPart] -> Bool
verifySig akey parts = and $ zipWith (checksig akey) bsigs bs
	where
		[kpart,mpart,spart] = sort parts
		[k,m,s] = map content [kpart,mpart,spart]
		msgh = hashf $ k `B.append` m
		bsigs = [B64.decode block | block <- B.split ',' s]
		bs = block 6 msgh
		sigtype = options spart !! 0
		hashtype = options spart !! 1
		checksig = fromJust $ M.lookup sigtype checksigs
		hashf = fromJust $ M.lookup hashtype hashfs

-- contains the hashfunctions, key is the value of the option in the MsgPart-Header
hashfs :: M.Map HashType (B.ByteString -> B.ByteString)
hashfs = M.fromList [("SHA256",SHA.hash)]

-- contains the "check signature" functions, key is the value of the option in the MsgPart-Header
checksigs :: M.Map AsymCipher (AsymKey -> B.ByteString -> B.ByteString -> Bool)
checksigs = M.fromList [("RSA",RSA.checksig)]

{--------------------
sample data and tests
--------------------}
runTests :: Bool
runTests = and [verifySig pub $ (genMsgPart "RSA" priv "SHA256" otherparts) : otherparts | (pub,priv) <- keys]
	where
		otherparts = [kcpart,msgcpart]
		kcpart = MsgPart KEYCRYPTED ["RSA"] "ourkeyourkeyourkeyourkeyourkey"
		msgcpart = MsgPart MSGCRYPTED ["SHA256","CBC"] "ourdataourdataourdataourdataourdata"

keys :: [(B.ByteString,B.ByteString)]
keys =	[("----BEGIN RSA PUBLIC KEY----\nAQAB,iUdRIBeyL3qX\n----END RSA PUBLIC KEY----"
		 ,"----BEGIN RSA PRIVATE KEY----\nH0vn/c/pBfHZ,iUdRIBeyL3qX\n----END RSA PRIVATE KEY----")
		,("----BEGIN RSA PUBLIC KEY----\nAQAB,Y9G9TSdJNf0j\n----END RSA PUBLIC KEY----"
		 ,"----BEGIN RSA PRIVATE KEY----\nH0jRB6FRS9Th,Y9G9TSdJNf0j\n----END RSA PRIVATE KEY----")
		,("----BEGIN RSA PUBLIC KEY----\nAQAB,Lfh0qKrNlchv\n----END RSA PUBLIC KEY----"
		 ,"----BEGIN RSA PRIVATE KEY----\nEVl5vcC88PQh,Lfh0qKrNlchv\n----END RSA PRIVATE KEY----")
		]

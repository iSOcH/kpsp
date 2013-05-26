module Kpspcrypto.KeyCrypted where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe

import Kpspcrypto.Msg

type AsymCipher = B.ByteString -- z.B. "RSA"
type AsymKey = B.ByteString -- kompletter inhalt des pubkeyfiles

genMsgPart :: AsymCipher -> AsymKey -> B.ByteString -> MsgPart
genMsgPart "RSA" = genMsgPartRSA

genMsgPartRSA :: AsymKey -> B.ByteString -> MsgPart
genMsgPartRSA akey skey = MsgPart KEYCRYPTED ["RSA"] skey

getSymKey :: AsymKey -> MsgPart -> B.ByteString
getSymKey akey msg = (fromJust $ M.lookup cipher ciphers) akey msg
	where
		cipher = head $ options msg 

getSymKeyFromRSA :: AsymKey -> MsgPart -> B.ByteString
getSymKeyFromRSA akey msg = content msg

ciphers :: M.Map B.ByteString (AsymKey -> MsgPart -> B.ByteString)
ciphers = M.fromList [("RSA", getSymKeyFromRSA)]

module Kpspcrypto.KeyCrypted where

-- Takes the arguments from the main files and prepares them for further use.
-- With the help of this functions Messageparts etc. for KeyCrypted 
-- message can be generated.
-- Are other Encryption Modes created, they need to be added here.

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe

import Kpspcrypto.Msg
import Kpspcrypto.Pad
import Kpspcrypto.Serial
import qualified Kpspcrypto.Base64 as B64
import qualified Kpspcrypto.RSA as RSA

-- creates a KEYCRYPTED-msgpart using the given asymmetric
-- cipher, the given key for the asymmetric cipher and
-- the given content in encrypted form
genMsgPart :: AsymCipher -> AsymKey -> B.ByteString -> MsgPart
genMsgPart "RSA" akey skey = MsgPart KEYCRYPTED ["RSA"] enckey
	where 
		enckeyed = map B64.encode [RSA.encrypt akey blocks | blocks <- block 4 skey]
		enckey = B.intercalate "," enckeyed 

--decodes the content of a KEYCRYPTED-part using the supplied key
getSymKey :: AsymKey -> MsgPart -> B.ByteString
getSymKey akey msg = (fromJust $ M.lookup cipher ciphers) akey msg
	where
		cipher = head $ options msg

--decodes the content of a KEYCRYPTED-part using RSA
getSymKeyFromRSA :: AsymKey -> MsgPart -> B.ByteString
getSymKeyFromRSA akey msg = B.concat [RSA.decrypt akey $ B64.decode block | block <- B.split ',' $ content msg]

--maps the option-value in the keycrypted-header to the function
--responsible for decoding the part
ciphers :: M.Map B.ByteString (AsymKey -> MsgPart -> B.ByteString)
ciphers = M.fromList [("RSA", getSymKeyFromRSA)]


{--------------------
sample data and tests
--------------------}
rsapubkey = "----BEGIN RSA PUBLIC KEY----\nBrk=,BAYh\n----END RSA PUBLIC KEY----" :: B.ByteString
rsaprivkey = "----BEGIN RSA PRIVATE KEY----\nBV0=,BAYh\n----END RSA PRIVATE KEY----" :: B.ByteString

-- reicht fuer 4 bytes :)
rsapriv2 = "----BEGIN RSA PRIVATE KEY----\nzFEWC0E=,AQro6bcX\n----END RSA PRIVATE KEY----" :: B.ByteString
rsapub2 = "----BEGIN RSA PUBLIC KEY----\nAQAB,AQro6bcX\n----END RSA PUBLIC KEY----" :: B.ByteString

ourdata = "ourdata" :: B.ByteString

simplegentest = genMsgPart "RSA" rsapriv2 ourdata
simplegetKeytest = getSymKey rsapub2 simplegentest

module Kpspcrypto.MsgCrypted (genMsgPart, getPlain) where

-- Takes the arguments from the main files and prepares them for further use.
-- With the help of this functions Messageparts etc. for MsgCrypted 
-- message can be generated.
-- Are other Encryption Modes created, they need to be added here.

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe
import System.Random
import Data.Char

import qualified Kpspcrypto.AES256 as AES
import qualified Kpspcrypto.Base64 as B64
import Kpspcrypto.Msg
import Kpspcrypto.BlockModes
import Kpspcrypto.Pad

type Key = B.ByteString
type SymCipher = B.ByteString
type ChainMode = B.ByteString

-- create a MSGCRYPTED-part using a random IV and a random Key, also
-- returns the used key for further usage (in the KEYCRYPTED part)
genMsgPart :: StdGen -> SymCipher -> ChainMode -> B.ByteString -> (MsgPart, Key)
genMsgPart rgen "AES256" "CBC" plain = (MsgPart MSGCRYPTED ["AES256","CBC"] (ivenc `B.append` "," `B.append` plainenc), key)
	where
		[key,iv] = rndStrs [32,16] rgen
		plainenc = B64.encode $ (cbc (AES.encode key) iv) plain
		ivenc = B64.encode iv
genMsgPart rgen "AES256" "ECB" plain = (MsgPart MSGCRYPTED ["AES256","ECB"] plainenc, key)
	where
		key = rndStr 32 rgen
		plainenc = B64.encode $ (ecb (AES.encode key) iv) plain
		--only length matters, must be the same as the blocksize of the cipher
		iv = B.replicate 16 '\0'

-- decodes the content of a MSGCRYPTED-part using the supplied key
getPlain :: Key -> MsgPart -> B.ByteString
getPlain key msg = (fromJust $ M.lookup cipher decodingfunctions) key msg
	where
		cipher = head $ options msg

-- decodes the content of a part encrypted using AES
getPlainFromAES :: Key -> MsgPart -> B.ByteString
getPlainFromAES key msg = (modef (AES.decode key) iv) cont
	where
		mode = options msg !! 1
		-- find the function which "unapplies" the block-chaining mode
		modef = fromJust $ M.lookup mode modes
		-- for ecb we have to supply a pseudo-iv, for cbc the iv is
		-- part of the content of the msgpart
		[iv, cont]	| mode == "ECB" = [B.replicate 16 '\0', B64.decode $ content msg]
					| mode == "CBC" = map B64.decode $ B.split ',' $ content msg

-- maps the option-value in the msgpart-header to the function
-- responsible for decoding a part
decodingfunctions :: M.Map B.ByteString (Key -> MsgPart -> B.ByteString)
decodingfunctions = M.fromList [("AES256", getPlainFromAES)]

-- maps the option-value in the msgpart-header to the function
-- responsible for "unapplying" the block-chaining
modes :: M.Map B.ByteString ((Block -> Block) -> IV -> B.ByteString -> B.ByteString)
modes = M.fromList [("CBC",uncbc), ("ECB",unecb)]

{-------------------------------
creating random keys, ivs etc...
-------------------------------}
-- takes a list of lengths and returns random ByteStrings with
-- these lengths created using the supplied random generator
rndStrs :: [Int] -> StdGen -> [B.ByteString]
rndStrs lengths gen = split lengths allrndstrs
	where
		split [] "" = []
		split (len:lens) tosplit = B.take len tosplit : (split lens (B.drop len tosplit))
		allrndstrs = rndStr (sum lengths) gen

-- creates a random ByteString with given length using the
-- supplied random generator
rndStr :: Int -> StdGen -> B.ByteString
rndStr n gen = B.pack $ rndCL n gen

-- creates a random String with given length using the
-- supplied random generator
-- the reason for creating this method is that prepending
-- single Chars to [Char] is way faster (O(1)) the same operation
-- than on a ByteString (O(n))
rndCL :: Int -> StdGen -> String
rndCL 0 gen = ""
rndCL n gen = chr rc : rndCL (n-1) newgen
	where
		(rc, newgen) = randomR (0,255) gen

{----
tests
----}
runTests :: Bool
runTests = and [testAESECB,testAESCBC]

contents :: [B.ByteString]
contents = 	["the very secret and hopefully somewhat protected plaintext"
			,"another, shorter text"
			,""
			,B.replicate 5000 't'
			]

testAESECB :: Bool
testAESECB = and [plain m == getPlain (key m) (msg m) | m <- msgskeys]
	where
		msgskeys = [(genMsgPart rnd "AES256" "ECB" cont,cont) | rnd <- rnds, cont <- contents]
		msg = fst . fst
		key = snd . fst
		plain = snd

testAESCBC :: Bool
testAESCBC = and [plain m == getPlain (key m) (msg m) | m <- msgskeys]
	where
		msgskeys = [(genMsgPart rnd "AES256" "CBC" cont,cont) | rnd <- rnds, cont <- contents]
		msg = fst . fst
		key = snd . fst
		plain = snd

rnds :: [StdGen]
rnds = [mkStdGen i | i <- [13..63]]

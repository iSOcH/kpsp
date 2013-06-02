module Kpspcrypto.MsgCrypted where

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


getPlain :: Key -> MsgPart -> B.ByteString
getPlain key msg = (fromJust $ M.lookup cipher plains) key msg
	where
		cipher = head $ options msg

getPlainFromAES :: Key -> MsgPart -> B.ByteString
getPlainFromAES key msg = (modef (AES.decode key) iv) . B64.decode $ cont
	where
		mode = options msg !! 1
		modef = fromJust $ M.lookup mode modes
		[iv, cont]	| mode == "ECB" = [B.replicate 16 '\0', content msg]
					| mode == "CBC" = B.split ',' $ content msg

plains :: M.Map B.ByteString (Key -> MsgPart -> B.ByteString)
plains = M.fromList [("AES256", getPlainFromAES)]

modes :: M.Map B.ByteString ((Block -> Block) -> IV -> B.ByteString -> B.ByteString)
modes = M.fromList [("CBC",uncbc), ("ECB",unecb)]

{-------------------------------
creating random keys, ivs etc...
-------------------------------}
rndStrs :: [Int] -> StdGen -> [B.ByteString]
rndStrs lengths gen = split lengths allrndstrs
	where
		split [] "" = []
		split (len:lens) tosplit = B.take len tosplit : (split lens (B.drop len tosplit))
		allrndstrs = rndStr (sum lengths) gen

rndStr :: Int -> StdGen -> B.ByteString
rndStr n gen = B.pack $ rndCL n gen

rndCL :: Int -> StdGen -> String
rndCL 0 gen = ""
rndCL n gen = chr rc : rndCL (n-1) newgen
	where
		(rc, newgen) = randomR (0,255) gen

{----
tests
----}
contents :: [B.ByteString]
contents = 	["the very secret and hopefully somewhat protected plaintext"
			,"another, shorter text"
			,""
			,B.replicate 1000 't'
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
rnds = [mkStdGen i | i <- [13..113]]

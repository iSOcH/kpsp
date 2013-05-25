module Kpspcrypto.MsgCrypted where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import System.Random
import Data.Char

import Kpspcrypto.Msg
import Kpspcrypto.AES256

type Key = B.ByteString
type SymCipher = B.ByteString
type ChainMode = B.ByteString

genMsgPart :: StdGen -> SymCipher -> ChainMode -> B.ByteString -> (MsgPart, Key)
genMsgPart rgen cipher mode plain = (MsgPart MSGCRYPTED [cipher,mode] plain, "ourkey")


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
rndStr 0 gen = ""
rndStr n gen = chr rc `B.cons` rndStr (n-1) newgen
	where
		(rc, newgen) = randomR (0,255) gen

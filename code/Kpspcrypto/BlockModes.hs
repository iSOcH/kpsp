module Kpspcrypto.BlockModes where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.Bits

import Kpspcrypto.Pad
import Kpspcrypto.Serial

type Block = B.ByteString
type IV = Block

cbc :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
cbc cipher iv plain = B.concat $ docbc cipher iv $ block blocklen plain
	where
		blocklen = B.length iv

docbc :: (Block -> Block) -> IV -> [Block] -> [Block]
docbc _ _ [] = []
docbc cipher iv (x:xs) = cblock : docbc cipher cblock xs
	where
		ivi = asInt iv
		xi = asInt x
		ivxorb = asStr $ ivi `xor` xi
		cblock = cipher ivxorb

uncbc :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
uncbc cipher iv crypt = B.concat $ douncbc cipher iv $ block blocklen crypt
	where
		blocklen = B.length iv

douncbc :: (Block -> Block) -> IV -> [Block] -> [Block]
douncbc _ _ [] = []
douncbc cipher iv (x:xs) = plain : douncbc cipher x xs
	where
		ivi = asInt iv
		xdec = asInt $ cipher x
		plain = asStr $ ivi `xor` xdec

ecb :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
ecb cipher iv plain = B.concat [cipher pblock | pblock <- pblocks]
	where
		blocklen = B.length iv
		pblocks = pad blocklen plain

unecb :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
unecb cipher iv crypt = unpadblocks [cipher cblock | cblock <- cblocks]
	where
		blocklen = B.length iv
		cblocks = block blocklen crypt

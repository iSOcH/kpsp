module Kpspcrypto.BlockModes where

-- Block Modes are needed for AES etc. 

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.Bits

import Kpspcrypto.Pad
import Kpspcrypto.Serial

type Block = B.ByteString
type IV = Block

-- cypher block chaining mode encryption Ci = Ek(Pi xor Ci-1)
-- Ci are crypted blocks, Pi plain blocks. We use the supplied IV as C0.
-- http://de.wikipedia.org/wiki/Cipher_Block_Chaining_Mode
-- This is the main function which seperates the supplied 
-- ByteStrings into Blocks and calls the helper function.
cbc :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
cbc cipher iv plain = B.concat $ docbc cipher iv $ pad blocklen plain
	where
		blocklen = B.length iv

-- helper function. The supplied Blocks of Bytestrings are
-- encoded by this function.
docbc :: (Block -> Block) -> IV -> [Block] -> [Block]
docbc _ _ [] = []
docbc cipher iv (x:xs) = cblock : docbc cipher cblock xs
	where
		ivi = asInt iv
		xi = asInt x
		ivxorb = asStr $ ivi `xor` xi
		cblock = cipher ivxorb

-- cypher block chaining mode decryption Pi = Dk(Ci) xor Ci-1
-- main and helper function to decrypt. Same way as encryption.
uncbc :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
uncbc cipher iv crypt = unpadblocks $ douncbc cipher iv $ block blocklen crypt
	where
		blocklen = B.length iv

douncbc :: (Block -> Block) -> IV -> [Block] -> [Block]
douncbc _ _ [] = []
douncbc cipher iv (x:xs) = plain : douncbc cipher x xs
	where
		ivi = asInt iv
		xdec = asInt $ cipher x
		plain' = asStr $ ivi `xor` xdec
		plainlengthmissing = B.length iv - B.length plain'
		plain = B.replicate plainlengthmissing '\0' `B.append` plain'

-- electronic codebook mode
-- https://en.wikipedia.org/wiki/Block_cipher_modes_of_operation#Electronic_codebook_.28ECB.29
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

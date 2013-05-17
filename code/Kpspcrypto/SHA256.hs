module Kpspcrypto.SHA256 (hash) where

-- http://en.wikipedia.org/wiki/SHA-2

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.Word
import Data.Bits
import Data.Char

-- apply SHA256 to given data, result will always be 32 bytes long
hash :: B.ByteString -> B.ByteString 
hash input = id input

-- adds padding (in the form 0x70[00]*) until there are 4 bytes left for the size
pad :: B.ByteString -> B.ByteString
pad input = fill $ input `B.snoc` chr 0x70
	where
		-- 56bytes are 448bits
		fill unfilled
			-- we fill up to 60 bytes because B.length only gives us 32bit
			| lenmod == 60 = unfilled
			| otherwise = unfilled `B.append` B.replicate remaining '\0'
		lenmod = (B.length input) + 1 `mod` 64 -- +1 because we already added a byte
		--124 because 62 `mod` 64 results in -2, which we cant use for replicate
		remaining = (124 - lenmod) `mod` 64

-- adds padding and size, output length will always be a multiple of 64 bytes
preprocess :: B.ByteString -> B.ByteString
preprocess input = pad input `B.append` lenAsBStr
	where
		len = B.length input
		-- extracts bytes from Int and packs them into ByteString
		lenAsBStr = B.pack
			[chr $ shiftR (len .&. 0xFF000000) 24
			,chr $ shiftR (len .&. 0x00FF0000) 16
			,chr $ shiftR (len .&. 0x0000FF00) 8
			,chr $ len .&. 0x000000FF
			]

getChunks :: B.ByteString -> [B.ByteString]
getChunks unchunked
	| B.length unchunked > 64 = first : getChunks rest
	| otherwise = [unchunked]
	where
		first = B.take 64 unchunked
		rest = B.drop 64 unchunked
{---
Data
---}
ks = 
	[0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
	,0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
	,0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
	,0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
	,0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
	,0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
	,0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
	,0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
	]

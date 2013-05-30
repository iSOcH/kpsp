module Kpspcrypto.SHA256 (hash) where

-- http://en.wikipedia.org/wiki/SHA-2

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.Word
import Data.Bits
import Data.Char

import Kpspcrypto.Pad
import Kpspcrypto.Serial

-- apply SHA256 to given data, result will always be 32 bytes long
hash :: B.ByteString -> B.ByteString 
hash msg = B.concat $ map w2b h
	where
		h = foldl perchunk hs preprocessed
		preprocessed = chunks $ preprocess msg

-- adds padding (in the form 0x80[00]*) until there are 4 bytes left for the size
shapad :: B.ByteString -> B.ByteString
shapad input = fill $ input `B.snoc` chr 0x80
	where
		-- 56bytes are 448bits
		fill unfilled
			| lenmod == 56 = unfilled
			| otherwise = unfilled `B.append` B.replicate remaining '\0'
		lenmod = (B.length input) + 1 `mod` 64 -- +1 because we already added a byte
		--120 because 62 `mod` 64 results in -2, which we cant use for replicate
		remaining = (120 - lenmod) `mod` 64

-- adds padding and size, output length will always be a multiple of 64 bytes
preprocess :: B.ByteString -> B.ByteString
preprocess input = shapad input `B.append` lenAsBStr
	where
		len = 8 * B.length input --in bits
		lenAsBStr = B.pack
			['\NUL','\NUL','\NUL','\NUL'
			,chr $ shiftR (len .&. 0xFF000000) 24
			,chr $ shiftR (len .&. 0x00FF0000) 16
			,chr $ shiftR (len .&. 0x0000FF00) 8
			,chr $ len .&. 0x000000FF
			]

perchunk :: [Word32] -> B.ByteString -> [Word32]
perchunk curhash chunk = zipWith (+) curhash looped
	where
		broken = map b2w $ block 4 chunk
		expanded = expandwords broken
		looped = mainloop 0 expanded curhash

mainloop :: Int -> [Word32] -> [Word32] -> [Word32]
mainloop 64 _ h = h
mainloop i w [a,b,c,d,e,f,g,h] = mainloop (i+1) w [temp2,a,b,c,newd,e,f,g]
	where
		s1 = (e `rotateR` 6) `xor` (e `rotateR` 11) `xor` (e `rotateR` 25)
		ch = (e .&. f) `xor` ((complement e) .&. g)
		temp = h + s1 + ch + ks!!i + w!!i
		newd = d + temp;
		s0 = (a `rotateR` 2) `xor` (a `rotateR` 13) `xor` (a `rotateR` 22)
		maj = (a .&. (b `xor` c)) `xor` (b .&. c)
		temp2 = temp + s0 + maj

-- expands the 16 Word32s to 64 Word32s, according to SHA256 spec
expandwords :: [Word32] -> [Word32]
expandwords cw
	| length cw == 64 = cw
	| otherwise = expandwords $ cw ++ [newword cw]

newword :: [Word32] -> Word32
newword cw = cw!!(i-16) + s0 + cw!!(i-7) + s1
	where
		i = length cw
		s0 = (cw!!(i-15) `rotateR` 7) `xor` (cw!!(i-15) `rotateR` 18) `xor` (cw!!(i-15) `shiftR` 3)
		s1 = (cw!!(i-2) `rotateR` 17) `xor` (cw!!(i-2) `rotateR` 19) `xor` (cw!!(i-2) `shiftR` 10)

-- converts 4 Bytes to a Word32
b2w :: B.ByteString -> Word32
b2w = fromInteger . asInt

w2b :: Word32 -> B.ByteString
w2b = asStr . toInteger

-- break input into 512bit blocks
chunks :: B.ByteString -> [B.ByteString]
chunks = block 64

{---
Data
---}
hs :: [Word32]
hs =
	[0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]

ks :: [Word32]
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

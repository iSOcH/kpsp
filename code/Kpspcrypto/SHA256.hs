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

-- adds padding and size, output will always be a multiple of 64 bytes in length
preprocess :: B.ByteString -> B.ByteString
preprocess input = pad input `B.append` lenAsBStr
	where
		len = B.length input
		lenAsBStr = B.pack
			[chr $ len .&. 0xFF000000
			,chr $ len .&. 0x00FF0000
			,chr $ len .&. 0x0000FF00
			,chr $ len .&. 0x000000FF
			]

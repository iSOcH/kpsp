module Kpspcrypto.Pad (pad, unpad, unpadblocks, block) where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.Char

-- separate input into blocks and (always!) add padding
pad :: Int -> B.ByteString -> [B.ByteString]
pad n input
	| B.length input >= n = next : pad n rest
	| otherwise = [input `B.append` (B.replicate padlen padchar)]
	where
		(next, rest) = B.splitAt n input
		padlen = n - B.length input
		padchar = chr padlen

block :: Int -> B.ByteString -> [B.ByteString]
block n "" = []
block n x = next : (block n rest)
	where
		(next,rest) = B.splitAt n x

unpad :: Int -> B.ByteString -> B.ByteString
unpad n input = unpadblocks $ block n input

unpadblocks :: [B.ByteString] -> B.ByteString
-- last block: at least one byte is padding
unpadblocks [x] = B.take padlen x
	where
		n = B.length x
		padlen = n - (ord $ B.last x)
-- not the last block: recursion on following blocks
unpadblocks (x:xs) = x `B.append` unpadblocks xs

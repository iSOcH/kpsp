module Kpspcrypto.Pad (pad, unpad, unpadblocks, block) where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.Char

-- separate input into blocks and (always!) add padding
-- if input length mod blocklength == 0 we add a block that
-- only contains padding
pad :: Int -> B.ByteString -> [B.ByteString]
pad n input
	-- not the last block, recurse on following blocks
	| B.length input >= n = next : pad n rest
	-- last block: add padding
	| otherwise = [input `B.append` (B.replicate padlen padchar)]
	where
		(next,rest) = B.splitAt n input
		padlen = n - B.length input
		padchar = chr padlen

-- split input into block of given length
block :: Int -> B.ByteString -> [B.ByteString]
block n "" = []
block n x = next : (block n rest)
	where
		(next,rest) = B.splitAt n x

-- undo "pad n", uses "unpadblocks"
unpad :: Int -> B.ByteString -> B.ByteString
unpad n input = unpadblocks $ block n input

-- remove the padding in the last block
unpadblocks :: [B.ByteString] -> B.ByteString
-- last block: at least one byte is padding, because
-- pad always adds a padding
unpadblocks [x] = B.take padlen x
	where
		n = B.length x
		padlen = n - (ord $ B.last x)
-- not the last block: recursion on following blocks
unpadblocks (x:xs) = x `B.append` unpadblocks xs

{----
tests
----}
--runTests :: Bool
--runTests = 

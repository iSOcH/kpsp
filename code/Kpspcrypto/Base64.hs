module Kpspcrypto.Base64 (encode, decode) where

-- http://www.haskell.org/haskellwiki/DealingWithBinaryData
-- http://en.wikipedia.org/wiki/Base64

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Bits
import Data.Maybe
import Data.Char

{---------------
public functions
---------------}
-- encode a given ByteString using Base64
-- the output length will be a multiple of 4
encode :: B.ByteString -> B.ByteString
encode input = encodeR $ addpad input

-- decode Base64 encoded content of a ByteString
-- input length must be a multiple of 4
decode :: B.ByteString -> B.ByteString
decode encoded = B.take resultlen decWithTrash
	where
		(unpadded, padlen) = unpad encoded
		decWithTrash = decodeR unpadded
		resultlen = B.length decWithTrash - padlen

{---------------
encoding helpers
---------------}

-- recursively substitute 3 bytes with the 4 bytes
-- that result from Base64-encoding
-- the padding length is required for marking the
-- amount of added padding in the final output
encodeR :: (B.ByteString, Int) -> B.ByteString
encodeR ("",_) = ""
encodeR (x,padlen)
	| B.length x /= 3 || padlen == 0 = subs next `B.append` encodeR (rest, padlen)
	-- otherwise: last 3 bytes and we have padding
	| otherwise =
		if padlen == 1 then B.init (subs x) `B.append` "="
		else B.take 2 (subs x) `B.append` "=="
	where
		(next,rest) = B.splitAt 3 x
		-- convert to 6-bit-values, find the corresponding char and
		-- convert the [Char] to a ByteString
		subs input = B.pack $ map (table V.!) (toB64BitGroups input)
			
-- splits a ByteString (with length 3) into four 6-bit values
toB64BitGroups :: B.ByteString -> [Int]
toB64BitGroups x = [
	shiftR (ord (B.index x 0)) 2,
	shiftL (ord (B.index x 0) .&. 3) 4 .|. shiftR (ord (B.index x 1)) 4,
	shiftL (ord (B.index x 1) .&. 15) 2 .|. shiftR (ord (B.index x 2)) 6,
	ord (B.index x 2) .&. 63]

-- expands an input to a multiple of 3 Bytes for processing
-- second element of tuple is the length of the added padding
addpad :: B.ByteString -> (B.ByteString, Int)
addpad x = (x `B.append` B.replicate padlen '\0', padlen)
	where
		len = B.length x
		padlen = (3 - len `mod` 3) `mod` 3

-- base64 index table
-- contains the allowed characters in the Base64 output
table :: V.Vector Char
table = V.fromListN 64 (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/'])

{---------------
decoding helpers
---------------}
-- combines 4 6-bit values into a ByteString with length 3
fromB64BitGroups :: [Int] -> B.ByteString
fromB64BitGroups x = B.pack $ map chr ords
	where
		ords = [
			shiftL (x !! 0) 2 .|. shiftR (x !! 1) 4,
			shiftL ((x !! 1) .&. 15) 4 .|. shiftR (x !! 2) 2,
			shiftL ((x !! 2) .&. 3) 6 .|. (x !! 3)]

-- removes the padding from a Base64-encoded input
-- second element in the returned tuple is the amount
-- of bytes to be discarded after decoding
unpad :: B.ByteString -> (B.ByteString, Int)
unpad x = (
	B.takeWhile (/= '=') x `B.append` B.replicate padlen '0',
	padlen )
	where
		padlen = B.length (B.dropWhile (/= '=') x)

-- recursively substitute 4 "Base64-Bytes" with 3 Bytes from
-- the plaintext which was encoded
decodeR :: B.ByteString -> B.ByteString
decodeR "" = ""
decodeR x = subs next `B.append` decodeR rest
	where
		(next,rest) = B.splitAt 4 x
		-- convert ByteString to [Char], restore the original 4
		-- 6-bit-values and convert them to the original 3 Bytes
		subs input = fromB64BitGroups [fromJust (M.lookup c tableR) | c <- B.unpack input]

-- contains the 6bit-values for the allowed chars in Base64 encoded data
tableR :: M.Map Char Int
tableR = M.fromList [(table V.! v ,v) | v <- [0..63]]

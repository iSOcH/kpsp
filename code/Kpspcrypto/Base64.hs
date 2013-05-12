module Kpspcrypto.Base64 where

-- http://www.haskell.org/haskellwiki/DealingWithBinaryData
-- http://en.wikipedia.org/wiki/Base64

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Bits
import Data.Maybe
import Data.Char

{-----------------
encoding functions
-----------------}
encode :: B.ByteString -> B.ByteString
encode input = encodeR $ addpad input

encodeR :: (B.ByteString, Int) -> B.ByteString
-- encodeR B.empty = ... gives ghc error "Qualified name in binding position"
encodeR (x,padlen)
	| x == B.empty = x
	| B.length x /= 3 || padlen == 0 = subs (B.take 3 x) `B.append` encodeR ((B.drop 3 x), padlen)
	-- otherwise: last 3 bytes and we have padding
	| otherwise =
		if padlen == 1 then B.init (subs x) `B.append` "="
		else B.init (B.init (subs x)) `B.append` "=="
	where
		subs input = B.pack $ map (table V.!) (toB64BitGroups input)
			
toB64BitGroups :: B.ByteString -> [Int]
toB64BitGroups x = [
	shiftR (ord (B.index x 0)) 2,
	shiftL (ord (B.index x 0) .&. 3) 4 .|. shiftR (ord (B.index x 1)) 4,
	shiftL (ord (B.index x 1) .&. 15) 2 .|. shiftR (ord (B.index x 2)) 6,
	ord (B.index x 2) .&. 63]

addpad :: B.ByteString -> (B.ByteString, Int)
addpad x = (x `B.append` B.replicate padlen '\0', padlen)
	where
		len = B.length x
		padlen = (3 - len `mod` 3) `mod` 3

-- base64 index table
table :: V.Vector Char
table = V.fromListN 64 (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/'])


{-----------------
decoding functions
-----------------}
fromB64BitGroups :: [Int] -> B.ByteString
fromB64BitGroups x = B.pack $ map chr ords
	where
		ords = [
			shiftL (x !! 0) 2 .|. shiftR (x !! 1) 4,
			shiftL ((x !! 1) .&. 15) 4 .|. shiftR (x !! 2) 2,
			shiftL ((x !! 2) .&. 3) 6 .|. (x !! 3)]

decode :: B.ByteString -> B.ByteString
decode encoded = B.take (lenWithTrash - (snd upad)) decWithTrash
	where
		upad = unpad encoded
		decWithTrash = decodeR . fst $ upad
		lenWithTrash = B.length decWithTrash

unpad :: B.ByteString -> (B.ByteString, Int)
unpad x = (
	B.takeWhile (/= '=') x `B.append` B.replicate padlen '0',
	padlen )
	where
		padlen = B.length (B.dropWhile (/= '=') x)

decodeR :: B.ByteString -> B.ByteString
decodeR x
	| x == B.empty = x
	| otherwise = subs (B.take 4 x) `B.append` decodeR (B.drop 4 x)
	where
		subs input = fromB64BitGroups [fromJust (M.lookup c tableR) | c <- B.unpack input]

tableR :: M.Map Char Int
tableR = M.fromList [(c,v) | v <- [0..63], c <- [table V.! v]]

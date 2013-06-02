module Kpspcrypto.Serial where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.Char

-- interpret the bytes of a ByteString as
-- Integer, result will always be positive
asInt :: B.ByteString -> Integer
asInt str = f . reverse $ B.unpack str
	where
		f "" = 0
		f (x:xs) = toInteger (ord x) + 256 * f xs

-- convert a positive Integer to a ByteString
asStr :: Integer -> B.ByteString
-- edge case here because we dont want 0 to become "", but
-- we need a ""-edge-case in the helper function
asStr 0 = B.singleton '\0'
asStr i = B.pack . reverse $ f i
	where
		f 0 = []
		f i = chr (fromInteger (i `mod` 256)) : f (i `div` 256)

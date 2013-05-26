module Kpspcrypto.BlockModes where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B

type Block = B.ByteString
type IV = Block

cbc :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
cbc cipher iv plain = id plain

uncbc :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
uncbc cipher iv crypt = id crypt

ecb :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
ecb cipher iv plain = id plain

unecb :: (Block -> Block) -> IV -> B.ByteString -> B.ByteString
unecb cipher iv crypt = id crypt

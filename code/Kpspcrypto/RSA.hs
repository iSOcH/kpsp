module Kpspcrypto.RSA (encrypt, sign, decrypt, checksign) where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B

-- first part is e or d, second is n
type Pubkey = (B.ByteString, B.ByteString)
type Privkey = (B.ByteString, B.ByteString)

encrypt :: Pubkey -> B.ByteString -> B.ByteString
encrypt (e, n) msg = id msg

sign :: PrivKey -> B.ByteString -> B.ByteString
sign (d, n) msg = id msg

decrypt :: PrivKey -> B.ByteString -> B.ByteString
decrypt (d, n) crypted = id crypted

checksign :: Pubkey -> B.ByteString -> B.ByteString -> Bool
checksign (e, n) sig msg = True

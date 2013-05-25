module Kpspcrypto.MsgCrypted where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import System.Random

import Kpspcrypto.Msg

type Key = B.ByteString
type SymCipher = B.ByteString
type ChainMode = B.ByteString

genMsgPart :: StdGen -> SymCipher -> ChainMode -> B.ByteString -> (MsgPart, Key)
genMsgPart rgen cipher mode plain = (MsgPart MSGCRYPTED [cipher,mode] plain, "ourkey")

-- was in diesem sinne, rekursiv: randomR (0,255) (mkStdGen 12314) :: (Int, StdGen)

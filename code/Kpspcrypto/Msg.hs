module Kpspcrypto.Msg where

-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Text.Regex.Posix

type AsymCipher = B.ByteString -- z.B. "RSA"
type AsymKey = B.ByteString -- kompletter inhalt des pub- oder privatekeyfiles
type HashType = B.ByteString -- z.B. "SHA256"

sampleMsgStr = 	"----BEGIN KEYCRYPTED RSA 8----\nbla//+ba\n----END KEYCRYPTED----\n\n" `B.append`
				"----BEGIN MSGCRYPTED AES256 CBC----\nbla+bl/ubb\n----END MSGCRYPTED----\n\n" `B.append`
				"----BEGIN SIGNATURE SHA256 RSA 8----\nbl/+ubb+/+lubb\n----END SIGNATURE----\n\n"

-- possible types of messageparts
data MsgType = KEYCRYPTED | MSGCRYPTED | SIGNATURE deriving (Show, Read, Eq)

-- this can hold any type of messagepart
data MsgPart = MsgPart	{	msgtype :: MsgType
						,	options :: [B.ByteString]
						,	content :: B.ByteString
						} deriving (Read)

makeMsg :: (MsgType, [B.ByteString]) -> B.ByteString -> MsgPart
makeMsg (msgtype, options) content = MsgPart msgtype options content

-- make the msg print in the way we want and expect it in a file
instance Show MsgPart where
	show msg = "----BEGIN " ++ show (msgtype msg) ++ " " ++
		B.unpack (B.intercalate " " (options msg)) ++
		"----\n" ++ B.unpack (content msg) ++ "\n" ++
		"----END " ++ show (msgtype msg) ++ "----"
-- alternative for intercalate: foldl (\acc option -> acc `B.append` " " `B.append` option) "" (options msg)

-- interprets the first line of a msgpart
-- TODO: make sure the first line actually starts with "----BEGIN "
readHdr :: B.ByteString -> (MsgType, [B.ByteString])
readHdr hdr = (msgtype, msgoptions)
	where
		-- drop 10 drops "----BEGIN "
		contents = B.words . B.takeWhile (/= '-') . B.drop 10
		msgtype = read . B.unpack . head $ contents hdr
		msgoptions = tail $ contents hdr

-- interpret a ByteString as MsgPart
-- the input has to be in the right form
readMsg :: B.ByteString -> MsgPart
readMsg input = makeMsg (readHdr headerLine) content
	where
		headerLine = head (B.lines input)
		-- outermost 'init' is for removing the trailing \n added by unlines
		content = B.init . B.unlines . init . tail $ B.lines input

-- gets messageparts from a file
-- hf explaining this...
-- http://stackoverflow.com/questions/7636447/raise-no-instance-for-regexcontext-regex-char-string
getMsgParts :: B.ByteString -> [MsgPart]
getMsgParts input = map readMsg regmatches
	where
		regmatches = getAllTextMatches (input =~ regex :: AllTextMatches [] B.ByteString)
		regex = "----BEGIN [A-Z0-9 ]+----\n[a-zA-Z0-9+/=]+\n----END [A-Z0-9]+----" :: B.ByteString

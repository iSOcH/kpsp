import Text.Regex.Posix

sampleMsgStr = 	"----BEGIN KEYCRYPTED RSA 8----\nbla//+ba\n----END KEYCRYPTED----\n\n" ++
				"----BEGIN MSGCRYPTED AES256----\nbla+bl/ubb\n----END MSGCRYPTED----\n\n" ++
				"----BEGIN SIGNATURE SHA256 RSA 8----\nbl/+ubb+/+lubb\n----END SIGNATURE----\n\n"

-- possible types of messageparts
data MsgType = KEYCRYPTED | MSGCRYPTED | SIGNATURE deriving (Show, Read, Eq)

-- this can hold any type of messagepart
data MsgPart = MsgPart	{	msgtype :: MsgType
						,	options :: [String]
						,	content :: String
						} deriving (Read)

makeMsg :: (MsgType, [String]) -> String -> MsgPart
makeMsg (msgtype, options) content = MsgPart msgtype options content

-- make the msg print in the way we want and expect it in a file
instance Show MsgPart where
	show msg = "----BEGIN " ++ show (msgtype msg) ++
		foldl (\acc option -> acc ++ " " ++ option) [] (options msg) ++ "----\n"
		++ content msg ++ "\n" ++
		"----END " ++ show (msgtype msg) ++ "----"

readMsg :: String -> MsgPart
readMsg input = makeMsg (readHdr headerLine) content
	where
		headerLine = head (lines input)
		-- outermost 'init' is for removing the trailing \n added by unlines
		content = init . unlines . init . tail $ lines input

-- interprets the first line of a msgpart
-- TODO: make sure the first line actually starts with "----BEGIN "
readHdr :: String -> (MsgType, [String])
readHdr hdr = (msgtype, msgoptions)
	where
		-- drop 10 drops "----BEGIN "
		contents = words . takeWhile (/= '-') . drop 10
		msgtype = read . head $ contents hdr
		msgoptions = tail $ contents hdr

-- gets messageparts from a file
-- hf explaining this...
-- http://stackoverflow.com/questions/7636447/raise-no-instance-for-regexcontext-regex-char-string
getMsgParts :: String -> [MsgPart]
getMsgParts input = map readMsg regmatches
	where
		regmatches = getAllTextMatches (input =~ "----BEGIN[A-Z0-9 ]+----\n[a-zA-Z0-9+/]+\n----END [A-Z0-9]+----" :: AllTextMatches [] String)

-- possible types of messageparts
data MsgType = KEYCRYPTED | MSGCRYPTED | SIGNATURE deriving (Show, Read)

-- this can hold any type of messagepart
data MsgPart = MsgPart	{	msgtype :: MsgType
						,	options :: [String]
						,	content :: String
						} deriving Read

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
		content = init (unlines (init (tail (lines input))))

-- interprets the first line of a msgpart
-- TODO: make sure the first line actually starts with "----BEGIN "
readHdr :: String -> (MsgType, [String])
readHdr hdr = (msgtype, msgoptions)
	where
		-- drop 10 drops "----BEGIN "
		contents = words . takeWhile (/= '-') . drop 10
		msgtype = read (head (contents hdr))
		msgoptions = tail (contents hdr)

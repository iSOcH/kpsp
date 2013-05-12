-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

-- runhaskell -XOverloadedStrings hsencrypt.hs params...

import System.Environment
import Kpspcrypto.Msg
import qualified Data.ByteString.Char8 as B

main = do
	args <- getArgs
	handleArgs args

handleArgs :: [String] -> IO()
handleArgs args = do	
	if length args /= 2 then do
		printUsage
	else do
		let senderpubkey = args !! 0
		let cryptfile = args !! 1
		putStr "PublicKey of Sender: "
		putStrLn senderpubkey
		putStr "Encrypted File: "
		putStrLn cryptfile
		putStrLn "result of getMsgParts:"
		cryptcontent <- B.readFile cryptfile
		print $ getMsgParts cryptcontent

printUsage :: IO()
printUsage = do
	putStrLn "you need to call this binary in this way:"
	putStrLn "hsdecrypt sendersPublicKey cryptedfile.txt"

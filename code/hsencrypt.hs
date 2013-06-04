-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/ bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

-- runhaskell -XOverloadedStrings hsencrypt.hs params...

import System.Environment
import System.Random
import qualified Data.ByteString.Char8 as B
import Kpspcrypto.Msg
import qualified Kpspcrypto.MsgCrypted as M
import qualified Kpspcrypto.KeyCrypted as K
import qualified Kpspcrypto.Signature as S

main = do
	args <- getArgs
	handleArgs $ map B.pack args
		
	
handleArgs :: [B.ByteString] -> IO()
handleArgs args = do	
	if length args /= 7 then do
		printUsage
	else do
		let asym = args !! 0
		let hash = args !! 1
		let sym = args !! 2
		let blockmode = args !! 3
		let ownprivkey = args !! 4
		let rcptpubkey = args !! 5
		let infile = args !! 6
		pubkey <- B.readFile $ B.unpack rcptpubkey
		privkey <- B.readFile $ B.unpack ownprivkey
		plainFileContent <- B.readFile $ B.unpack infile
		rgen <- getStdGen
		let (mMsgPart, symkey) = M.genMsgPart rgen sym blockmode plainFileContent  --generates the crypted Message
		let kMsgPart = K.genMsgPart asym pubkey symkey  --generates the crypted Key
		let plainS = [kMsgPart,mMsgPart]
		let sMsgPart = S.genMsgPart asym privkey hash $ plainS  --generates the signature
		let msgParts = map (B.pack . show) [kMsgPart,mMsgPart,sMsgPart]
		B.writeFile (B.unpack infile ++ "Encrypted") $ B.intercalate "\n\n" msgParts

printUsage :: IO()
printUsage = do
	putStrLn "you need to call this binary in this way:"
	putStrLn "hsencrypt asymCipher hashAlg symCipher chainingMode privKey publicKey plainFile"
	putStrLn "Example: hsencrypt RSA SHA256 AES256 CBC privkey pubkey plaintext.txt"
	


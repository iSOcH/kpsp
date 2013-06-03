-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

-- runhaskell -XOverloadedStrings hsencrypt.hs params...

import System.Environment
import Data.List

import Kpspcrypto.Msg
import qualified Kpspcrypto.KeyCrypted as K
import qualified Kpspcrypto.MsgCrypted as M
import qualified Kpspcrypto.Signature as S
import qualified Data.ByteString.Char8 as B

main = do
	args <- getArgs
	handleArgs args

handleArgs :: [String] -> IO()
handleArgs args = do	
	if length args /= 3 then do
		printUsage
	else do
		[ourprivkey, senderpubkey, cryptcontent] <- mapM B.readFile args
		let parts@[keypart,msgcpart,sigpart] = sort $ getMsgParts cryptcontent
		let sigOK = S.verifySig senderpubkey parts
		if sigOK then do
			let symkey = K.getSymKey ourprivkey keypart
			let plaintext = M.getPlain symkey msgcpart
			if "Encrypted" `isSuffixOf` (args !! 2) then do
				let plainFile = dropEnd 9 $ args !! 2
				B.writeFile plainFile plaintext
			else do
				putStrLn "Output File?"
				plainFile <- getLine
				B.writeFile plainFile plaintext
		else do
			putStrLn "signature was wrong, exiting..."

printUsage :: IO()
printUsage = do
	putStrLn "you need to call this binary in this way:"
	putStrLn "hsdecrypt yourPrivKey sendersPublicKey cryptedfile.txt"

dropEnd :: Int -> [a] -> [a]
dropEnd n = reverse . drop n . reverse

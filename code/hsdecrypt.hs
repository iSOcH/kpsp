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
	--if we get to few arguments give a hint (printUsage) what we need.
	if length args /= 3 then do
		printUsage
	else do
		--maps the supplied arguments for further use.
		[ourprivkey, senderpubkey, cryptcontent] <- mapM B.readFile args
		let parts@[keypart,msgcpart,sigpart] = sort $ getMsgParts cryptcontent
		--before decryption check if signature is OK. Otherwise file got changed. 
		let sigOK = S.verifySig senderpubkey parts
		if sigOK then do
			let symkey = K.getSymKey ourprivkey keypart
			let plaintext = M.getPlain symkey msgcpart
			--check file ends with "encrypted". If so cut it and
			--save the plain file under the same name. Otherwise 
			--let the user choose a filename.
			if "Encrypted" `isSuffixOf` (args !! 2) then do
				let plainFile = dropEnd 9 $ args !! 2
				B.writeFile plainFile plaintext
			else do
				putStrLn "Output File?"
				plainFile <- getLine
				B.writeFile plainFile plaintext
		else do
			putStrLn "signature or key was wrong, exiting..."

printUsage :: IO()
printUsage = do
	putStrLn "you need to call this binary in this way:"
	putStrLn "hsdecrypt yourPrivKey sendersPublicKey cryptedfile.txt"

dropEnd :: Int -> [a] -> [a]
dropEnd n = reverse . drop n . reverse

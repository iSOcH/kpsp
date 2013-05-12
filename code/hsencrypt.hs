-- needed for using string-literals with ByteString
-- see http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Char8.html
{-# LANGUAGE OverloadedStrings #-}

-- runhaskell -XOverloadedStrings hsencrypt.hs params...

import System.Environment
import Kpspcrypto.Msg

main = do
	args <- getArgs
	handleArgs args

handleArgs :: [String] -> IO()
handleArgs args = do	
	if length args /= 6 then do
		printUsage
	else do
		let asym = args !! 0
		let hash = args !! 1
		let sym = args !! 2
		let blockmode = args !! 3
		let rcptpubkey = args !! 4
		let infile = args !! 5
		putStr "Asym Cipher: "
		putStrLn asym
		putStr "Hash Alg: "
		putStrLn hash
		putStr "Sym Cipher: "
		putStrLn sym
		putStr "Block Chaining Mode: "
		putStrLn blockmode
		putStr "Public Key of Recipient: "
		putStrLn rcptpubkey
		putStr "File with Plaintext: "
		putStrLn infile

printUsage :: IO()
printUsage = do
	putStrLn "you need to call this binary in this way:"
	putStrLn "hsencrypt asymCipher hashAlg symCipher chainingMode publicKey plainFile"
	putStrLn "Example: hsencrypt RSA SHA256 AES256 CBC pubkey plaintext.txt"

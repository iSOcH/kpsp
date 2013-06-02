import System.Environment
import Kpspcrypto.RSAKey
import System.Random

import qualified Data.ByteString.Char8 as B

main =
	do	gen <- newStdGen
		putStrLn "Enter filename:"
		fileName <- getLine
		B.writeFile (fileName ++ "RsaPrivKey") $ getPrivK $ genK gen
		B.writeFile (fileName ++ "RsaPubKey") $ getPubK $ genK  gen
			where
					getPrivK (x, _) = x
					getPubK (_, y) = y
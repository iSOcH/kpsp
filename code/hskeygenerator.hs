import System.Environment
import Kpspcrypto.RSAKey
import System.Random

import qualified Data.ByteString.Char8 as B

--main function to generate the private and public RSA key. 
--gen is need to generate random prime numbers in RSAKey.hs
--(p and q for the RSA N-Module). The keys are written
--to two seperate Files with the specific ending.  
main =
	do	gen <- newStdGen
		putStrLn "Enter filename:"
		fileName <- getLine
		B.writeFile (fileName ++ "RsaPrivKey") $ getPrivK $ genK gen
		B.writeFile (fileName ++ "RsaPubKey") $ getPubK $ genK  gen
			where
					getPrivK (x, _) = x
					getPubK (_, y) = y
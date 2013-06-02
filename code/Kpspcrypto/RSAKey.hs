module Kpspcrypto.RSAKey (genK) where

import qualified Data.ByteString.Char8 as B
import System.Random
import Kpspcrypto.Serial
import qualified Kpspcrypto.Base64 as B64


type P = Integer
type Q = Integer
type Privkey = B.ByteString
type Pubkey = B.ByteString

genK :: StdGen -> (Privkey, Pubkey)
genK rgen = (genPrivK rgen, genPubK rgen)

genPrivK :: StdGen -> Privkey
genPrivK rgen = begin `B.append`  toStr (getD $ genKeys rgen) `B.append` ","  `B.append` toStr (getN $ genKeys rgen) `B.append` end
		where 
			begin = "----BEGIN RSA PRIVATE KEY----\n"
			end = "\n----END RSA PRIVATE KEY----"
			getN :: (Integer, Integer) -> Integer
			getN (_, n) = n		--oder getN = snd	
			getD :: (Integer, Integer) -> Integer
			getD (d, _) = d		--oder getD = fst
			
genPubK :: StdGen -> Pubkey
genPubK rgen = begin `B.append`  toStr 65537  `B.append` ","  `B.append` toStr (getN $ genKeys rgen) `B.append` end
		where 
			begin = "----BEGIN RSA PUBLIC KEY----\n"
			end = "\n----END RSA PUBLIC KEY----"
			getN :: (Integer, Integer) -> Integer
			getN = snd


genKeys :: StdGen -> (Integer, Integer)
genKeys rgen = (d, n)
		where 
			p = head $ genPrime getP
			q = head $ genPrime getQ
			(getP, newGen) = (randomR (2^34, 2^35-1) rgen)  -- different range for p and q to ensure p!=q
			(getQ, newGen') = (randomR (2^35, 2^37) newGen)
			d = genD p q
			n = p*q 


--calculate d (decoding) e * d = 1 mod phi(N)
genD :: P -> Q -> Integer
genD p q = if scnd (extendedEuclid 65537 phiN) > 0 then scnd (extendedEuclid 65537 phiN) else phiN + scnd (extendedEuclid 65537 phiN)
	where
		phiN = (p-1)*(q-1)
		scnd (_, x, _) = x

--helper functions
extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a b 
				| b == 0 = (a, 1, 0)
				| otherwise = (d,t,s - (div a b)*t)
					where 
						(d,s,t) = extendedEuclid b (a `mod` b)
						
--first version with pattern matching -> chose guards to do sth different for once ;)
--extendedEuclid a 0 = b == 0 = (a, 1, 0)
--extendedEuclid a b = (d,t,s - (div a b)*t)
--					where 
--						(d,s,t) = extendedEuclid b (a `mod` b)

--create a list containing primes to get random p and q
genPrime :: Integer -> [Integer]
genPrime n = if even n then genPrime (n+1) else [x | x <- [n,n+2..], isPrime x]
		


isPrime :: Integer -> Bool
isPrime x = null [y | y <- takeWhile (\y -> y*y <= x) [2..], x `mod` y == 0]


-- converts an Integer to Base64 encoded ByteString
toStr :: Integer -> B.ByteString
toStr = B64.encode . asStr
module Kpspcrypto.RSAKey where

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
genPrivK rgen = begin `B.append`  toStr2 (getD $ genKeys rgen) `B.append` ","  `B.append` toStr2 (getN $ genKeys rgen) `B.append` end
		where 
			begin = "----BEGIN RSA PRIVATE KEY----"
			end = "----END RSA PRIVATE KEY----"
			getN :: (Integer, Integer) -> Integer
			getN (_, n) = n			
			getD :: (Integer, Integer) -> Integer
			getD (d, _) = d
			
genPubK :: StdGen -> Pubkey
genPubK rgen = begin `B.append`  toStr2 65537  `B.append` ","  `B.append` toStr2 (getN $ genKeys rgen) `B.append` end
		where 
			begin = "----BEGIN RSA PUBLIC KEY----"
			end = "----END RSA PUBLIC KEY----"
			getN :: (Integer, Integer) -> Integer
			getN (_, n) = n


genKeys :: StdGen -> (Integer, Integer)
genKeys rgen = (d, n)
		where 
			p = helperF !! getP
			q = helperF !! getQ
		--	p = head helperF
		--	q = head (drop 1 helperF)
			helperF = genPrime
			d = genD p q
			n = genModuleN p q 
			(getP, newGen) = (randomR (1, 49605) rgen)
			(getQ, newGen') = (randomR (1, 49606) newGen)
	
--create RSA-Modul (N = q*p)
genModuleN :: P -> Q -> Integer
genModuleN p q = if p /= q && p*q > 4294967296 then p*q else genModuleN p q


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
genPrime :: [Integer]
--genPrime = [x | x <- 700002:700003:[700005, 700007..7000000], isPrime x] n > 32 bit
genPrime = [x | x <- 4294967295:4294967297:[4294967299, 4294967301..4295967301], isPrime x] -- n > 64 bit

isPrime :: Integer -> Bool
isPrime x = null [y | y <- takeWhile (\y -> y*y <= x) [2..], x `mod` y == 0]


-- converts an Integer to Base64 encoded ByteString
toStr2 :: Integer -> B.ByteString
toStr2 = B64.encode . asStr
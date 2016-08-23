module Arithmetic where

import Data.List

--Problem 31
isPrime :: (Integral a) => a -> Bool
isPrime x = (factorList x !! 0) * (factorList x !! 1) == x

factorList :: (Integral a) => a -> [a]
factorList x = lowFactors ++ highFactors
    where lowFactors = filter isFactored [1..highEnd]
          highEnd = ceiling $ sqrt $ fromIntegral x
          highFactors = map (x `div`) lowFactors
          isFactored y = x `rem` y == 0

--Problem 32
myGCD :: (Integral a) => a -> a -> a
myGCD x 0 = abs x
myGCD x y = myGCD y (x `rem` y)

--Problem 33
coprime :: (Integral a) => a -> a -> Bool
coprime x y = gcd x y == 1

--Problem 34
totient :: (Integral a) => a -> a
totient 1 = 1
totient x = fromIntegral $ length $ filter (True == ) $ map (coprime x) [1..x]

--Problem 35
primeFactors :: (Integral a) => a -> [a]
primeFactors x = sort $ primeFactors' x
    where primeFactors' 1 = []
          primeFactors' x = maxPrimeFactor : primeFactors (x `div` maxPrimeFactor)
          maxPrimeFactor = last $ sort $ filter isPrime $ factorList x
          
--Problem 36
primeFactorsMult :: (Integral a) => a -> [(a, a)]
primeFactorsMult x = map (\y -> (head y, fromIntegral $ length y)) . group . sort $ primeFactors x

--Problem 37
totient' :: (Integral a) => a -> a
totient' x = foldr (*) 1 $ map phi $ primeFactorsMult x
    where phi (p, m) = (p - 1) * p  ^ (m - 1)
    
    
--Problem 39
primesR :: (Integral a) => a -> a -> [a]
primesR low high = filter isPrime [low..high]

--Problem 40
--goldbach :: (Integral a) :: a -> [a]
--goldbach x = 

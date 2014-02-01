module Primes (nondeterministicMillerRabin,
               deterministicMillerRabin) where

{- Helper function for Miller Rabin Test
 -}
extract2 :: (Integral a) => a -> (a, a)
extract2 = extract2' 0 
    where extract2' :: (Integral a) => a -> a -> (a, a)
          extract2' s d = if rem d 2 == 0
                             then extract2' (s+1) (div d 2)
                             else (s, d)

parameterList :: (Integral a) => a -> [a]
parameterList n 
              | n < 1373653 = [2, 3]
              | n < 9080191 = [31, 73]
              | n < 4759123141 = [2, 7, 61]
              | n < 2152302898747 = [2, 3, 5, 7, 11]
              | n < 3474749660383 = [2, 3, 5, 7, 11, 13]
              | n < 341550071728321 = [2, 3, 5, 7, 11, 13, 17]
              | otherwise  = [2..logL]
              where logL = floor $2 * (logBase 2 (fromIntegral n)) ** 2

-- Computes a^{ 2^r d} (mod n)
testCongruence :: (Integral a) => a -> a -> a -> a
testCongruence n a r = rem (a^(2^r * d)) n
        where (s,d) = extract2 (n-1)

-- Checks if the equation above is equals to -1 mod n, ie if
-- the result of the congruence is n-1
innerMRTest :: (Integral a) => a -> a -> a -> Bool
innerMRTest n a r = testCongruence n a r == n-1

-- returns false if the number is composite, must be checked 
-- various times to ensure a number is prime
-- Actually is the nondeterministic Miller Rabin version
nondeterministicMillerRabin :: (Integral a) => a -> a -> Bool
nondeterministicMillerRabin n a 
                            | rem (a^d) n == 1 = True
                            | any (innerMRTest n a) [0..(s-1)] = True
                            | otherwise = False
                                where (s,d) = extract2 (n-1)

deterministicMillerRabin :: (Integral a) => a -> Bool
deterministicMillerRabin n 
            | n == 2 = True
            | even n = False
            | n  > 2 = any (nondeterministicMillerRabin n) (parameterList n)

-- Naive prime algorithm
primes' = filter deterministicMillerRabin (2:[3,5..])

-- Better sieve algorithm, a vaguely optimized Sieve of Erathostenes
-- See the paper "Lazy wheel sieves and spirals of primes"
primes = sieveOfErathostenes wheel

sieveOfErathostenes (p:xs) = p: [x | x <- xs, noFactorIn primes squares x]

noFactorIn (p:ps) (q:qs) x = q > x || x `rem` p > 0 && noFactorIn ps qs x

squares = [p*p | p <- primes]

-- Primitive fixes wheel factorization
wheel = dropWhile (<=1) $ wheel'
    where wheel' = [z | x <- [0..], y <- [2,3,5,7,11,13,17,19,23,29], 
                       let z = x+y]

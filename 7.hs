extract2 :: (Integral a) => a -> (a, a)
extract2 = extract2' 0 
    where extract2' :: (Integral a) => a -> a -> (a, a)
          extract2' s d = if mod d 2 == 0
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
testCongruence n a r = mod (a^(2^r * d)) n
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
                            | mod (a^d) n == 1 = True
                            | any (innerMRTest n a) [0..(s-1)] = True
                            | otherwise = False
                                where (s,d) = extract2 (n-1)

determinisicMillerRabin :: (Integral a) => a -> Bool
determinisicMillerRabin n 
            | n == 2 = True
            | even n = False
            | n  > 2 = any (nondeterministicMillerRabin n) (parameterList n)

primes = filter determinisicMillerRabin (2:[3,5..])

main = putStrLn $ show $ primes !! 10001

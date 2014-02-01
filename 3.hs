import Debug.Trace
testNumber = 600851475143

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
-- TODO refactor to use this
testCongruence :: (Integral a) => a -> a -> a -> a
testCongruence n a r = mod (a^(2^r * d)) n
        where (s,d) = extract2 (n-1)

-- Checks if the equation above is equals to -1 mod n, ie if
-- the result of the congruence is n-1
innerMRTest :: (Integral a) => a -> a -> a -> Bool
innerMRTest n a r = mod (a^((2^r)*d)) n == n-1
        where (s,d) = extract2 (n-1)

-- returns false if the number is composite, must be checked 
-- various times to ensure a number is prime
-- Actually is the nondeterministic Miller Rabin version
outerMRTest :: (Integral a) => a -> a -> Bool
outerMRTest n a 
            | mod (a^d) n == 1 = True
            | any (innerMRTest n a) [0..(s-1)] = True
            | otherwise = False
        where (s,d) = extract2 (n-1)

millerRabin :: (Integral a) => a -> Bool
millerRabin n 
            | n == 2 = True
            | even n = False
            | n  > 2 = any (outerMRTest n) (parameterList n)

primes = filter millerRabin (2:[3,5..])

-- Takes a number and a list of possible divisors
-- returns the largest prime divisor
recDiv :: (Integral a, Ord a, Eq a) => a -> [a] -> a
recDiv _ []   = error "No divisors supplied!"
recDiv n (x:xs) = trace ("n: " ++ (show n) ++ "  x: " ++ (show x)) $
-- recDiv n (x:xs) = 
                if mod n x == 0
                then if millerRabin (div n x)
                        then (div n x)
                    else recDiv (div n x) (x:xs)
                else recDiv n xs

main = do
        -- putStrLn $ show $ zip [1..20 ] $ map extract2 [1..20]
        -- putStrLn $ show $ zip [2..2000] $ map millerRabin [2..2000]
        putStrLn $ show $ recDiv testNumber primes

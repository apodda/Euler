-- millerRabin :: Integer -> Boolean
-- millerRabin n
--     | even n || n <= 1 = false
--     | otherwise = not (any testMR [2..limit])
--         where limit = min (n-1) (truncate logL)
--                logL = 2 * (logBase 2 (fromInteger n)) ** 2
--              (s, d) = extract2 (n-1)
--          testMR a n = (mod (a^d) n != 1) && (test' n (s-1))
--           test' a s = all (test'' a) [1..s]
--          test'' a r = mod (a^(2^r*d)) n != -1
-- 
-- extract2 :: Integer -> (Integer, Integer)
-- extract2 n = extract2' 0 n
--     where extract2' :: Integer -> Integer -> (Integer, Integer)
--           extract2' s d = if mod d 2 == 0
--                              then extract2' (s+1) (div d 2)
--                              else (s, d)

-- Some constants
odds = map (1+) $ map (2*) [1..]
prime = 600851475143

modtest :: Integer -> (Integer -> Bool)
modtest n = \m ->
        let y = if (mod n m == 0) 
                then True
                else False
        in y

lessTest :: Integer -> (Integer -> Bool)
lessTest n =  \m -> (m <= n)

naivePrimeTest :: Integer -> Bool
naivePrimeTest n 
    | n == 1 = False
    | n == 2 = True
    | even n = False
    | otherwise = not $ any (modtest n) $ takeWhile (lessTest $ ceiling $ sqrt $ fromIntegral n) odds

main = do
         --putStrLn $ show $ zip [1..17] $ map naivePrimeTest [1..17]
         putStrLn $ show $ head $ dropWhile naivePrimeTest $ reverse [2..prime]

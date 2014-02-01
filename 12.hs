import Data.List

triang = scanl1 (+) [1..]

divby :: Integer -> Integer -> Bool
divby n m = (mod n m) == 0

-- divisors n = filter (n `divby`) [1..n]
divisors 1 = [1]
divisors n = union [1] $ union d $ map (b*) d
        where b = head $ filter (n `divby`) [2..n]
              c = n `div` b
              d = divisors c

main = do
        putStrLn $ show $ take 30 triang
        putStrLn $ show $ divby 4 2
        --putStrLn $ show $ take 10000 $ map (length . divisors) triang
        putStrLn $ show $ head $ filter 
                 (\n -> (length $ divisors n) > 499) triang

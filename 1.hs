sumMulti :: Integer -> Integer
sumMulti n = sum [ i | i <- [0..n] , mod i 3 == 0, mod i 5 == 0]

main = putStrLn (show (sumMulti 1000))

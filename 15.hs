import Debug.Trace

gridRoutes :: (Integral a) => a -> a -> a
--gridRoutes m 0 =  1
--gridRoutes 0 n =  1
--gridRoutes m 1 =  m
--gridRoutes 1 n =  n
--gridRoutes m n = (gridRoutes (m-1) n) + (gridRoutes m (n-1))
gridRoutes m n 
           | n == 0 || m == 0 = 1
           | m == 1 = n
           | n == 1 = m
           | n == m = 2 * gridRoutes (m-1) n
           | otherwise = (gridRoutes (m-1) n) + (gridRoutes m (n-1))

main = putStrLn $ show $ gridRoutes 20 20

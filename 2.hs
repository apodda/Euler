-- Standard dumb memoized Fibonacci generator
memo_fib:: Integer -> Integer
memo_fib0 = 1
memo_fib1 = 2
memo_fib  = fib'
    where fib' 0 = 1
          fib' 1 = 2
          fib' n = (memo_fib(n-1)) + (memo_fib(n-2))

-- Returns a list of Fibonacci numbers lesser than n
smallFib' :: Integer -> [Integer]
smallFib' n = takeWhile (<= n) (map memo_fib [])

-- Alternate Fibonacci generator, returns an infinite list
fib_list :: [Integer] -> [Integer]
fib_list xs = map head (iterate fib_gen xs)
    where fib_gen [m,n] = [n, m+n]

main = putStrLn $ show $ sum $ filter even $ takeWhile (<= 4000000) $ fib_list [1,2]

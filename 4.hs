str2Int :: String -> Int
str2Int n = read n :: Int

revNum :: Int -> Int
revNum = (str2Int . reverse . show)

isPalindrome :: Int -> Bool
isPalindrome n = (n == revNum n)

products = [ i*j | i <- [100..999], j <-[100..999] ]

main = do
        putStrLn $ show $ maximum $ filter isPalindrome products

import Prelude

-- Define Euler totient function
euler :: (Num n) => n -> n
euler n = length [ d | d <- [1..n] ,gcd n d == 1 ]

-- Define divisors function
-- FIXME addinf 'let' seems to solve 'out of scope errors'
tau :: (Num n) => n -> n
tau n = length [ d | d <- [1..n] , gcd n d == d  ]

solutions = [ n | euler n == tau n, n <- [1..] ]

strsol = map show (takeWhile (<10000) solutions)

-- IO functions, shamelessly copied from RealWorldHaskell

list2actions :: [String] -> [IO ()]
list2actions = map str2action
        where str2action input = putStrLn ("Data: " ++ input)

runall :: [IO ()] -> IO ()
runall [] = return ()
runall (firstelem:remainingelems) = 
    do firstelem
       runall remainingelems

main = do runall ( list2actions strsol )
         

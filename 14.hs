import Control.Monad.State
import Debug.Trace

f n
  | even n = div n 2
  | odd  n = 3*n + 1

collatz :: Int -> [Int]
collatz n = takeWhile (==1) $ iterate f n

-- some refactoring needed
maxExtract :: [Int] -> (Int -> Int) -> State Int Int
maxExtract [] _ = do
        state <- get
        return state
maxExtract (x:xs) f = do
        state <- get
        if (f state) > (f x)
            then put state
        else put x
        trace ("State=" ++ (show state)
            ++ " f(state)=" ++ (show  (f state))
            ++ " f(x)=" ++ (show  (f x)))
            $ maxExtract xs f

-- main

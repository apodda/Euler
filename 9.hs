main = putStrLn $ show $ product $
        [ [x, y, z] | z <- [1..998], x <- [1..z],
        let y = (1000 - z - x), y > x, x^2 + y^2 == z^2 ] !! 0

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isprime :: Int -> Bool
isprime x = null divs
   where divs = [ y | y <- [2..(isqrt x)] , x `mod` y == 0]

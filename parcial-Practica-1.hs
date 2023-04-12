-- (1a)
sinA :: [Char] -> [Char]
sinA [] = []
sinA (x:xs) |(x /= 'a') && (x /= 'A') = x : sinA xs
            |otherwise = sinA xs

-- (2a)
checkSum :: [(Int,Int,Int)] -> [Bool]
checkSum [] = []
checkSum ((x,y,z):xs) = ((x + y) == z) : checkSum xs
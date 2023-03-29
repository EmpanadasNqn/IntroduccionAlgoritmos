
-- (3a) Funcion Filter para guardar pares
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | (mod x 2 == 0) = x : soloPares xs
                 | otherwise = soloPares xs

-- (3b) Funcion Filter para guardar mayores que 10
mayorQue10 :: [Int] -> [Int]
mayorQue10 [] = []
mayorQue10 (x:xs) | (x > 10) = x : mayorQue10 xs
                  | otherwise = mayorQue10 xs

-- (3c) Funcion Filter para guardar Numeros mayores que un numero n
mayoresQue :: Int -> [Int] -> [Int]
mayoresQue y [] = []
mayoresQue y (x:xs) | (x > y) = x : mayoresQue y xs
                    | otherwise = mayoresQue y xs

-- (4a) Funcion Map 
sumar1 :: [Int] -> [Int]
sumar1 [] = []
sumar1 (x:xs) = (x + 1) : sumar1 xs

-- (4b)  Funcion Map 
duplicar :: [Int] -> [Int]
duplicar [] = []
duplicar (x:xs) = (x * 2) : duplicar xs

-- (4c)  Funcion Map 
multiplicar :: Int -> [Int] -> [Int]
multiplicar y [] = []
multiplicar y (x:xs) = (x * y) : multiplicar y xs

-- (5a)
todosMenores10 :: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (x:xs) = (x < 10) == todosMenores10 xs

-- (5b)
hay0 :: [Int] -> Bool
hay0 [] = True
hay0 (x:xs) = (x == 0) == hay0 xs

-- (5c)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- (6)

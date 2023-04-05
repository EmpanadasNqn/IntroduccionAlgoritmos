
-- (3a) Funcion Recursiva Filter para guardar pares
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | (mod x 2 == 0) = x : soloPares xs
                 | otherwise = soloPares xs

-- (3b) Funcion Recursiva Filter para guardar mayores que 10
mayorQue10 :: [Int] -> [Int]
mayorQue10 [] = []
mayorQue10 (x:xs) | (x > 10) = x : mayorQue10 xs
                  | otherwise = mayorQue10 xs

-- (3c) Funcion Recursiva Filter para guardar Numeros mayores que un numero n
mayoresQue :: Int -> [Int] -> [Int]
mayoresQue y [] = []
mayoresQue y (x:xs) | (x > y) = x : mayoresQue y xs
                    | otherwise = mayoresQue y xs

-- (4a) Funcion Recursiva Map para sumar 1
sumar1 :: [Int] -> [Int]
sumar1 [] = []
sumar1 (x:xs) = (x + 1) : sumar1 xs

-- (4b)  Funcion Recursiva Map para multiplicar por 2
duplicar :: [Int] -> [Int]
duplicar [] = []
duplicar (x:xs) = (x * 2) : duplicar xs

-- (4c)  Funcion Recursiva Map para multiplicar por n
multiplicar :: Int -> [Int] -> [Int]
multiplicar y [] = []
multiplicar y (x:xs) = (x * y) : multiplicar y xs

-- (5a) Funcion Recursiva Fold para saber si toda una lista es menor a 10
todosMenores10 :: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (x:xs) = (x < 10) == todosMenores10 xs

-- (5b) Funcion Recursiva Fold para saber si hay 0 en la lista
hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x:xs) = (x == 0) || hay0 xs

-- (5c) Funcion Recursiva Fold de sumatoria
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- (6) Funcion Recursiva Zip
repartir :: [String] -> [String] -> [(String,String)]
repartir [] ys = []
repartir (x:xs) [] = (x," No hay cartas en el mazo rey") : repartir xs []
repartir (x:xs) (y:ys) = (x,y) : repartir xs ys

-- (7) Funcion Recursiva UnZip
apellidos :: [(String, String, Int)] -> [String]
apellidos [] = []
apellidos ((x,y,z):xs) = y : apellidos xs

-- (8) Funcion Length definida recursivamente
lengthRecursiva :: [Int] -> Int
lengthRecursiva [] = 0
lengthRecursiva (x:xs) = 1 + length xs

-- (8) Funcion !! definida recursivamente
exclamacionRecursiva :: [Int] -> Int -> Int
exclamacionRecursiva (x:xs) 0 = x
exclamacionRecursiva (x:xs) y = exclamacionRecursiva xs (y - 1)

-- (8) Funcion take definida recursivamente
takeRecursiva :: Int -> [Int] -> [Int]
takeRecursiva 0 xs = []
takeRecursiva y (x:xs) = x : takeRecursiva (y - 1) xs

-- (8) Funcion drop definida recursivamente
dropRecursiva :: Int -> [Int] -> [Int]
dropRecursiva 0 xs = xs
dropRecursiva y (x:xs) = dropRecursiva (y - 1) xs

-- (8) Funcion ++ definida recursivamente
sumsumRecursiva :: [Int] -> [Int] -> [Int]
sumsumRecursiva [] ys = ys
sumsumRecursiva (x:xs) ys = x : sumsumRecursiva xs ys

-- (9a) Funcion Filter maximo de una lista
maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

-- (9b) Funcion Fold
suma :: Int -> Int
suma x = 0

-- Ejercicio 1.
-- a.
-- Función para ver si un número es 0.
esCero :: Int -> Bool
esCero n = n == 0

-- b.
-- Función para ver si un número es positivo.
esPositivo :: Int -> Bool
esPositivo n = n > 0

-- c.
-- Función para ver si un carácter es una vocal.
esVocal :: Char -> Bool
esVocal c = c `elem` "aeiou"

-- Ejercicio 2.
-- a.
paraTodo [] = True
paraTodo (x:xs) = x && paraTodo xs

-- b.
-- Función para sacar la sumatoria de todos los enteros de una lista.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- c.
-- Función para multiplicar todos los números de la lista.
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- d.
-- Función para calcular el factorial de un número.
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

-- e.
-- Función para sacar promedio de una lista de números.
promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (length xs)

-- Ejercicio 3.
-- Función para ver si un entero está en una lista.
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- Ejercicio 4.
-- a.
-- Función para ver si algún elemento de la lista xs satisfacen el predicado t.
paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] t = True
paraTodo' (x:xs) t = t x && paraTodo' xs t

-- b.
-- Función para ver si algún elemento de la lista xs satisfacen el predicado t.
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = t x || existe' xs t

-- c.
-- Función para aplicara a la lista el predicado t sumar los elementos de la lista resultante.
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t

-- d.
-- Función para aplicara a la lista el predicado t sumar los elementos de la lista resultante.
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = t x * productoria' xs t

-- Ejercicio 5.
-- Definir la función paraTodo pero usando solamente paraTodo'.
paraTodo'' :: [Bool] -> Bool
paraTodo'' xs = paraTodo' xs (== True)

-- Ejercicio 6.
-- a.
-- Definir una función para saber si todos los elementos de una lista son pares usando las funciones del ejercicio 4.
todosPares :: [Int] -> Bool
todosPares xs = paraTodo' xs even

--b.
-- Definir una función para saber si dentro de una lista hay un múltiplo de un número.
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (esMultiplo n)
-- Definición de una función auxiliar que me devuelve si un número x es múltiplo de un número y.
esMultiplo :: Int -> Int -> Bool
esMultiplo x y = x `mod` y == 0

-- c.
-- Definir una función que sume los primeros n cuadrados.
sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [1..x] (^2)

-- d.
-- La función factorial la podría redefinir usando la función productoria.
factorial2 :: Int -> Int
factorial2 x = productoria [1..x]

-- e.
-- Función para multiplicar pares en una lista de enteros.
multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' xs esParO1
-- Función para saber si un número es par para desarrollar la función de multiplicaPares.
esParO1 :: Int -> Int
esParO1 x 
   | esMultiplo x 2 = x
   | otherwise = 1

-- Ejercicio 7.
-- a) Función map: La función map aplica una función 'f' a cada elemento de una lista 'xs' y devolver la lista resultante. De ahí viene el nombre 'map' haciendo referencia a mapear cada elemento de una lista para devolver otra de la misma longitud.
-- Función filter: La fumodule Proyecto1 where
duplica (x:xs) = 2*x : duplica xs
-- Con Map.module Proyecto1 where
duplicaMap :: [Int] -> [Int]
duplicaMap = map (*2)

-- Ejercicio 9.
--Programá una función que dada una lista de n ́umeros xs, calcula una lista que tiene como elementos aquellos n ́umeros de xs que son pares.
-- a) Usando recursión.
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) 
   | even x = x : soloPares xs
   | otherwise = soloPares xs

-- b) Usando filter:
soloParesFilter :: [Int] -> [Int]
soloParesFilter = filter even

-- c) Mejorando función del punto 6e:
multiplicaPares' :: [Int] -> Int
multiplicaPares' xs = productoria' (soloParesFilter xs) (*1)

-- Ejercicio 10.
-- La función 'primIgualesA' toma un valor y una lista, y calcula el tramo inicial m ́as largo de la lista cuyos elementos son iguales a ese valor.
-- a) Programar la función usando recursión.
primIgualesA :: Ord a => a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) 
   |n == x = x : primIgualesA n xs
   |otherwise = []

-- b) Programar la función usando 'takeWhile'.
primIgualesA' :: Ord a => a -> [a] -> [a]
primIgualesA' n = takeWhile (== n)

-- Ejercicio 11.
-- La función primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos elementos son todos iguales entre sí.
-- a) Programar la función por recursión.
primIguales :: Ord a => [a] -> [a]
primIguales [] = []
primIguales [a] = [a]
primIguales (x:y:xs)
   |x == y = x : primIguales (y:xs)
   |otherwise = [x]
-- b) Definir otra versión de 'primIguales' usando cualquier versión de primIgualesA.
primIguales' :: Ord a => [a] -> [a]
primIguales' xs = primIgualesA' (head xs) xs

-- Ejercicio 12.
-- Definir de manera recursiva la función 'cuantGen' (denota la cuantificación generalizada):
cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen _ z [] _ = z
cuantGen op z (x:xs) t = op (t x) (cuantGen op z xs t)
-- Definición de las funciones del punto cuatro sin usar recursión y en una línea por función.
-- a.
-- Función para ver si todos los elementos de la lista xs satisfacen el predicado t.
paraTodo''' :: [a] -> (a -> Bool) -> Bool
paraTodo''' xs t = cuantGen (&&) True xs t
-- b.
-- Función para ver si algún elemento de la lista xs satisfacen el predicado t.
existe'' :: [a] -> (a -> Bool) -> Bool
existe'' xs t = cuantGen (||) False xs t
-- c.
-- Función para aplicara a la lista el predicado t sumar los elementos de la lista resultante.
sumatoria'' :: [a] -> (a -> Int) -> Int
sumatoria'' xs t = cuantGen (+) 0 xs t
-- d.
-- Función para aplicara a la lista el predicado t sumar los elementos de la lista resultante.
productoria'' :: [a] -> (a -> Int) -> Int
productoria'' xs t = cuantGen (*) 1 xs t

-- Ejercicio 13.
-- a.
-- f :: (a, b) -> ...
-- f (x , y) = ...
-- La función 

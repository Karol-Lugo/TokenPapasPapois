module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

-- Recursion de cola: el acumulador lleva el producto parcial.
potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = potencia' base exponente 1

potencia' :: Int -> Int -> Int -> Int
potencia' _ 0 acc = acc
potencia' base exponente acc =
  potencia' base (exponente - 1) (acc * base)

-- CPS: la continuacion recuerda "multiplicar por base" lo que regrese.
potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1
potenciaCPS base exponente k =
  potenciaCPS base (exponente - 1) (\v -> k (base * v))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitud' xs 0

longitud' :: [a] -> Int -> Int
longitud' [] acc = acc
longitud' (_ : xs) acc = longitud' xs (acc + 1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (_ : xs) k = longitudCPS xs (\v -> k (1 + v))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = reversa' xs []

reversa' :: [a] -> [a] -> [a]
reversa' [] acc = acc
reversa' (x : xs) acc = reversa' xs (x : acc)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x : xs) k = reversaCPS xs (\v -> k (v ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xss = concatena' xss []

concatena' :: [[a]] -> [a] -> [a]
concatena' [] acc = acc
concatena' (xs : xss) acc = concatena' xss (acc ++ xs)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs : xss) k = concatenaCPS xss (\v -> k (xs ++ v))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = intercala' xs ys []

intercala' :: [a] -> [a] -> [a] -> [a]
intercala' [] ys acc = acc ++ ys
intercala' xs [] acc = acc ++ xs
intercala' (x : xs) (y : ys) acc = intercala' xs ys (acc ++ [x, y])

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x : xs) (y : ys) k =
  intercalaCPS xs ys (\v -> k (x : y : v))


module Practica1 where

getGreater :: (Int,Int) -> (Int,Int) -> (Int,Int)
getGreater a b = ((comparar a), (comparar b))

comparar ::  (Int ,Int) -> Int
comparar x = if(fst x >= snd x) then (fst x) else (snd x)

less :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
less a b c = ((comparar3 a), (comparar3 b), (comparar3 c))

comparar3 :: (Int, Int, Int) -> Int
comparar3 (a, b, c) = if(a<=b) then (if(a <=c) then a else c) else (if(b<=c) then b else c)

isPar :: (Int, Int, Int, Int) -> Bool
isPar (a,b,c,d) = (mod a 2 == 0) && (mod b 2== 0) && (mod c 2==0) && (mod d 2==0)

count :: [Int] -> [(Int, Int)]
count [] = []
count (x:xs) = [(x, contar x (x:xs))]++ count xs


contar :: Int -> [Int] -> Int
contar a [] = 0
contar a (x:xs) = if(a == x) then 1 + (contar a xs) else 0 + (contar a xs)
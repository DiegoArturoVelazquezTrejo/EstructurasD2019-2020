module  Practica3 where

getHead :: [a] -> a
getHead [] = error "No hay elementos"
getHead (x:xs) = x


getLast :: [a] -> a
getLast [] = error "No hay elementos"
getLast (x:[]) = x
getLast (x:xs) = getLast xs

lengthList :: [a] -> Int
lengthList [] = 0
lengthList (x:xs) = 1 + lengthList xs

addAll :: [Int] -> Int
addAll [] = 0
addAll (x:xs) = x + addAll xs

inversion :: [a] -> [a]
inversion [] = []
inversion (x:xs) = (inversion xs)++[x]

isEmpty :: [a] -> Bool
isEmpty x = lengthList x == 0

getMaximun :: [Int] -> Int
getMaximun [] = error "No hay elementos"
getMaximun (x:[]) = x
getMaximun (x:xs) = if(x < head xs) then getMaximun xs else getMaximun ([x]++ tail xs)

{-  1
Función: pairs
Recibe un índice 'n' y regresa la lista de los primeros 'n' números primos.
-}
pairs :: Int -> [Int]
pairs n = mul [0 .. (n-1)] 2

mul :: [Int] -> Int -> [Int]
mul [] n = []
mul (x:xs) n = [x * n] ++ mul xs n
{-  2
Función: rotate
Recibe un índice 'n' y una lista.
Regresa la lista después de rotar (ponerlos al final de la lista) los primeros 'n' elementos.
-}

rotate :: Int -> [a] -> [a]
rotate (-1) a = a
rotate n (x:xs) = (rotate (n-1) (xs++[x]))

{-  3
Función: palindrome
Recibe una lista y regresa True si la lista es palindrome, False en otro caso.
-}
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome (x:xs) = inversa (x:xs) == (x:xs)

inversa :: [a] -> [a]
inversa [] = []
inversa (x:xs) = inversa xs ++ [x]

{-  4
Función: intercalate
Recibe un elemento y una lista. Regresa la lista de listas con el elemento intercalado en cada lista.
-}
intercalate :: a -> [a] -> [[a]]
intercalate a [] = [[a]]
intercalate a (x:xs) = [insertar (x:xs) a 0]


-- [4,1,2,3] [1,4,2,3] [1,2,4,3] [1,2,3,4]

insertar :: [a] -> a -> Int -> [a]
insertar (x:xs) elemento 0 = [elemento]++(x:xs)
insertar (x:xs) elemento indice = [x]++insertar xs elemento (indice -1)


{-  5
Función: permutations
Recibe una lista y regresa todas las posibles permutaciones de los elementos.
-}
permutations :: [a] -> [[a]]
permutations a = combinaciones a  a


combinaciones :: [a] -> [a] -> [[a]]
combinaciones xs ys = [[x]++[y] | x <- xs, y <- ys]

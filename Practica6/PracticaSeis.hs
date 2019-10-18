module Practica6 where

-- Función que te dice si un número pertenece o no a la lista
-- Te regresa true si pertenece y false si no pertenece.
belongs :: Int -> [Int] -> Bool
belongs a [] = False
belongs a (x:xs)= if(a == x) then True else belongs a xs

-- Función que te regresa el elemento en el índice dado dentro de la lista
get_nth :: Int -> [a] -> a
get_nth n [] = error "No hay elementos"
get_nth 0 (x:xs) = x
get_nth n (x:xs) = get_nth (n-1) xs

-- Función que elimina el elemento del índice dado sobre la lista
delete_nth :: Int -> [a] -> [a]
delete_nth n [] = error "La lista es vacía"
delete_nth 0 (x:xs) = xs
delete_nth n (x:xs) = [x]++(delete_nth (n-1) xs)

-- FUnción que elimina todos los números repetidos sobre la lista.
-- El primer paŕametro es el elemento a eliminiar y el segundo es la lista.
delete_all :: (Eq a) => a -> [a] -> [a]
delete_all a [] = []
delete_all a (x:xs) = if(a == x) then (delete_all a xs) else [x]++(delete_all a xs)

-- Función que regresa los elementos que cumplen con la condición booleana que se pasa como argumento.
filterLab :: (a -> Bool) -> [a] -> [a]
filterLab p [] = []
filterLab p (x:xs) =if(p x) then [x]++ (filterLab p xs) else filterLab p xs



data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

leaf a = Node a Empty Empty


-- Función que te dice el número de elementos de un árbol binario.
num_elem :: BTree a -> Int
num_elem Empty = 0
num_elem (Node a b c) = 1 + (num_elem b) + (num_elem c)


-- FUnción que te dice el número de hojas de un arbol binario.
num_leaves :: BTree a -> Int
num_leaves Empty = 0
num_leaves (Node a Empty Empty) = 1
num_leaves (Node a b c) = (num_leaves b) + (num_leaves c)

-- FUnción que te dice si un elemento pertenece al arbol binario o no.
belongsT :: (Eq a) => a -> BTree a -> Bool
belongsT z Empty = True
belongsT z (Node a Empty Empty) = if(z == a) then True else False
belongsT z (Node a b c) = if(z == a) then True else (belongsT z b) || (belongsT z c)

-- Función que te ordena los elementos de un arbol binario en preorder regresándote una lista ordenada.
preorder :: BTree a -> [a]
preorder Empty = []
preorder (Node a b c) = [a] ++ (postorder b) ++ (postorder c)

-- Función que te ordena los elementos de un arbol binario en inorder regresándote una lista ordenada.
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node a b c) = (inorder b) ++ [a] ++ (inorder c)

-- Función que te ordena los elementos de un arbol binario en postorder regresándote una lista ordenada.
postorder :: BTree a -> [a]
postorder Empty = []
postorder (Node a b c) = (postorder b) ++ (postorder c) ++ [a]


-- Función que te agrega un elemento al arbol binario preservando el orden que tenía previamente.
add :: (Eq a, Ord a) => a -> BTree a -> BTree a
add a Empty = (Node a Empty Empty)
add z (Node a b c) = if(belongsT z (Node a b c)) then (Node a b c) else if(z < a) then (Node a (add z b) c) else (Node a b (add z c))


-- 1Función que te elimina el elemento que ingreses como argumento preservando el orden del arbol binario.
delete :: (Eq a, Ord a) => a -> BTree a -> BTree a
delete z (Node a Empty Empty) = if(z == a) then Empty else (Node a Empty Empty)
delete z (Node a Empty c) = if(a == z) then c else (Node a Empty (delete z c))
delete z (Node a b Empty) = if(a == z) then b  else (Node a (delete z b) Empty)
delete z (Node a (Node x y q) (Node t u v)) = if(z == a) then (Node x (delete z (Node a y q)) (Node t u v))else
  if (z < a) then (Node a (delete z (Node x y q)) (Node t u v))
  else (Node a (Node x y q) (delete z (Node t u v)))

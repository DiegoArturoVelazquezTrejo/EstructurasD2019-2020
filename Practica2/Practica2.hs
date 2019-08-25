module Practica2 where

{-  1
Función: fibo
Recibe un entero n y regresa el fibonacci de n. -}
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)


{-  2
Función: factorial
Recibe un entero n y regresa el factorial de n. -}
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n -1)

{-  3
Función: triangle_number
Recibe un entero n y regresa la suma de los n números naturales
	desde 1 hasta n.
-}
--A esto se le conoce como número triangular.
triangle_number :: Int -> Int
triangle_number 0 = 0
triangle_number a = a + triangle_number (a-1)

{-  4
Función: power
Recibe un número x y un exponente n y regresa el resultado de x^n
-}
power :: Int -> Int -> Int
power a 0 = 1
power a 1 = a
power a b = a * power a (b-1)

-- Función que recibe un entero y te regresa 0 si es par y 1 si es impar
a2 :: Int -> Int
a2 0 = 0
a2 1 = 1
a2 n = if(n>0) then a2 (n-2) else a2 (n+2)

{-  5
Función: isPair
Recibe un número n. Regresa True cuando n es par, False en otro caso.
-}
isPair :: Int -> Bool
isPair a = if(a2 a == 0) then True else False

{-  6
Función: isOdd
Recibe un número n. Regresa True cuando n es impar, False en otro caso.
-}
isOdd :: Int -> Bool
isOdd a = if(a2 a == 1) then True else False

{-  7
Función: add_pairs
Recibe un número n y regresa la suma de los enteros positivos desde 2 hasta n.
Si n es impar entonces mandar mensaje de error.
-}
add_pairs :: Int -> Int
add_pairs a = if(isOdd a) then error "El numero es impar" else (triangle_number a) -1

{-  Inasistentes a clase
Función: mult
Recibe dos números 'a' y 'b', regresa el resultado de a*b.
	La multiplicación debe ser una suma recursiva, es decir: a*b = a*a*a*... tantas veces como b
	Ejemplo: 2*5 = 2*2*2*2*2
-}
mult :: Int -> Int -> Int
mult a 0 = 0
mult a 1 = a
mult a b = a + mult a (b-1)

{-  EXTRA (cp)
Función: division
Recibe dos números 'a' y 'b', regresa el resultado de a/b.
	La división debe ser mediante resta.
	Ejemplo: division 10 2 representa
			           10/2 = 1+((10-2)/2) = 1+(8/2)                      Aquí se ejemplifica la resta a-b
										   = 1+(1+(6/2))
										   = 1+(1+(1+(4/2)))
										   = 1+(1+(1+(1+(2/2))))
										   = 1+(1+(1+(1+(1+(0/2)))))
										   = 1+(1+(1+(1+(1+0)))
										   = 5
-}
-- Hint: en la llamada recursiva deberán restar 'b' a 'a'. En otras palabras; a = a-b
division :: Int -> Int -> Int
division 1 a = 0
division 0 a = 0
division a 1 = a
division a n = 1 + division (a - n) n

to_binary :: Int -> [Int]
to_binary 0 = [0]
to_binary 1 = [1]
to_binary n = to_binary (division n 2) ++ [mod n 2]

add_digits :: Int -> Int
add_digits 0 = 0
add_digits 1 = 1
add_digits n = (mod n 10) + add_digits (div n 10)

descomposicion1 :: Int -> [Int]
descomposicion1 1 = [1]
descomposicion1 2 = [1,1]
descomposicion1 n = descomposicion1 (n-1) ++ [1]

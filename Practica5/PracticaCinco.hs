module Pracica5 where

data Natural = Cero | S Natural 

instance Show Natural where
  show(Cero) = "0"
  show(S n) = show(to_int (S n))

-- Convierte un natural a un tipo de natural
to_nat :: Int -> Natural
to_nat 0 = Cero
to_nat n = if(n<0) then error"El número no es natural " else S (to_nat(n-1))

-- Va a sumar dos números de tipo Natural
addition :: Natural -> Natural -> Natural
addition n Cero = n
addition n (S m) =  addition (S n) m

-- Va a multiplicar dos naturales de tipo Natural
mult :: Natural -> Natural -> Natural
mult n Cero = Cero
mult n (S (Cero)) = n
mult n (S m) = addition n (mult n m)

-- Comparará dos números de tipo Natural, si el primero es mayor, regresa true, de lo contrario, regresa false
lt :: Natural -> Natural -> Bool
lt Cero Cero = False
lt n Cero = True
lt Cero n = False
lt (S n) (S m) = lt n m

-- Recibe una lista de números de tipo Natural y regresa el más pequeño
min_nat :: [Natural] -> Natural
min_nat [x] = x
min_nat (x:xs) = if(not(lt x (head xs))) then min_nat (x:tail xs) else min_nat xs

-- Convierte un número de tipo Natural a int
to_int :: Natural -> Int
to_int Cero = 0
to_int (S n) = 1 + to_int n

-- Realiza la sucesión de fibonacci con números de tipo Natural
fib :: Natural -> Natural
fib Cero = Cero
fib (S Cero) = (S Cero)
fib (S (S(n))) = addition (fib(S n))  (fib n)

-- Realiza el factorial del número de tipo Natural
fac :: Natural -> Natural
fac (S Cero) = (S Cero)
fac (S n) =  mult (S n) (fac n)

-- Realiza la resta de dos números de tipo Natural
substraction :: Natural -> Natural -> Natural
substraction n Cero = n
substraction (S n) (S m) =  substraction n m

-- Realiza la división de dos números de tipo Natural
division :: Natural -> Natural ->Natural
division n Cero = error "Estás tonto"
division Cero n = Cero
division n (S Cero) = n
division m n = addition (S Cero) (division (substraction m n) n)

-- Regresa True si la lista de Naturales está ordenada, False si no está ordenada
-- Función que regresa True si los elementos de la lista estan ordenados de mayor a menor, False en otro caso.
isordered :: [Natural] -> Bool
isordered [x] = True
isordered (x:xs) = if(lt x (head xs)) then isordered xs else False

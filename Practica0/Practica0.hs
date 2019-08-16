module Practica0 where

-- Función addThree
-- Recibe tres números y regresa la suma de ellos.
addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

-- Función div recibe dos números y regresa la división de los mismos
di :: Int -> Int -> Int
di a b = div a b

-- Función minlab
-- Recibe dos números como parámetros y regresa el menor
minlab :: Int -> Int -> Int
minlab a b = if(a > b) then b else a

-- Función month recibe un número y regresa el mes del año que corresponde
month ::Int -> String
month x = if(x <= 12) then ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"] !!(x - 1)
        else "EL año tiene solo 12 meses"

-- Función and1 regresa el resultado de aplicar el conectivo and logico a los dos valores booleanos que recibe.
andl :: Bool -> Bool -> Bool
andl a b = a && b

-- Función isTriange recibe tres valores y regresa un booleano diciendo si es o no triángulo.
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = if((((a+b)>c) ||  ((a+c)>b)) || ((b+c)>a))  then True else False

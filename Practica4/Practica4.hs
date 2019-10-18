module Practica5 where

-- Dato para representar expresiones aritméticas
data EA = N Int | Var String | Pos EA | Neg EA | Sum EA EA | Res EA EA | Mult EA EA | Div EA EA | Modulo EA EA | Pot EA EA 


instance Show EA where
 show(N a) = show a
 show(Var c) = c
 show(Pos ea) = "(+" ++show ea++ ")"
 show(Neg ea) = "(-" ++show ea ++ ")"
 show(Sum ea1 ea2) = show ea1 ++ " + "++ show ea2
 show(Res ea1 ea2) = show ea1 ++ " - " ++ show ea2
 show(Mult ea1 ea2) = show ea1 ++ " x " ++ show ea2
 show(Div ea1 ea2) = show ea1 ++ " / " ++ show ea2
 show(Modulo ea1 ea2) = show ea1 ++ " % " ++ show ea2
 show(Pot ea1 ea2) = show ea1 ++ " ^ " ++ show ea2


-- Ejercicio 1: Operador binario --
pasarsumaEA :: Int -> Int -> EA
pasarsumaEA a b = Sum (N a) (N b)

-- Ejercicio 2 --
pasarrestaEA :: Int -> Int -> EA
pasarrestaEA a b = Res (N a) (N b)

-- Ejercicio 3 --
pasarmultEA :: Int -> Int -> EA
pasarmultEA a b = Mult (N a) (N b)

-- Ejercicio 4 --
pasardivEA :: Int -> Int -> EA
pasardivEA a b = Div (N a) (N b) 

-- Ejercicio 5 --
pasarmoduloEA :: Int -> Int -> EA
pasarmoduloEA a b = Modulo (N a) (N b) 

-- Ejercicio 6 --
pasarpotEA :: Int -> Int -> EA
pasarpotEA a b= Pot (N a) (N b)

-- Ejercicio 8: Operadores unarios --
mayorqueUnario :: EA -> EA -> Bool
mayorqueUnario (Pos (N a)) (Neg (N b)) = True
mayorqueUnario (Neg (N a)) (Pos (N b)) = False
mayorqueUnario (Pos (N a)) (Pos (N b)) = a>b
mayorqueUnario (Neg (N a)) (Neg (N b)) = a<b  

-- Ejercicio 9: Evaluacion --
--eval (Res (Sum (Sum (N 2) (Mult (N 3) (N 9))) (Mult (N 4) (N 2))) (Div (Mult (N 7) (N 4)) (N 2)))
--23
eval :: EA -> Int
eval (N n) = n 
eval (Var s) = error "no existe para este caso"
eval (Pos ea) = eval ea
eval (Neg ea) = - eval ea
eval (Res a b) = eval a - eval b 
eval (Sum a b) = eval a + eval b
eval (Mult a b)= eval a * eval b
eval (Div a b) = div (eval a) (eval b)
eval (Pot ea1 ea2) = eval ea1 ^ eval ea2
eval (Modulo ea1 ea2) = mod (eval ea1) (eval ea2)

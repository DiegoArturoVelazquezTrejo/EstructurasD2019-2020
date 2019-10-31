module PracticaSiete where

--
data Prop = PTrue | PFalse | Var Nombre | Neg Prop | Conj Prop Prop | Disy Prop Prop | Impl Prop Prop | Syss Prop Prop

type Nombre = String
type Asignacion = [(String, Prop)]

instance Show Prop where
 show (PTrue) = "True"
 show (PFalse) = "False"
 show (Var x) = x
 show (Neg p) = "¬ " ++ show p
 show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
 show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
 show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
 show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Ejercicio 2
-- let e = (Syss (Impl (Var "A") (Var "B")) (Impl (Var "C") (Var "D")))
-- let e2 = (Syss (Impl (Var "A") (Var "B")) (Impl (Var "C") (Conj (Var "A") (Var "C"))))

-- Función que elimina los elementos repetidos de una lista
-- Su parámetro es una lista de tipo Nombre y regresa una lista de tipo Nombre
eliminaRepetidos :: [Nombre] -> [Nombre]
eliminaRepetidos [x] = [x]
eliminaRepetidos (x:xs) = if(elem x xs) then eliminaRepetidos xs else [x]++(eliminaRepetidos xs)

-- Regresa las variables que tiene una proposición P eliminando las repetidas de la lista
variables :: Prop -> [Nombre]
variables lprop = eliminaRepetidos(variablesA lprop)

-- Función auxiliar que regresa las variables de una proposición P
variablesA :: Prop -> [Nombre]
variablesA (Var x) = [x]
variablesA (Neg x) = variables x
variablesA (Conj p q) = (variablesA p) ++ (variablesA q)
variablesA (Disy p q) = (variablesA p) ++ (variablesA q)
variablesA (Impl p q) = (variablesA p) ++ (variablesA q)
variablesA (Syss p q) = (variablesA p) ++ (variablesA q)

-- Ejercicio 3
-- let e3 = (Conj (Conj (Var "A") (Var "B")) (Var "C"))
-- let e4 = (Disy (Disy (Var "A") (Var "B")) (Var "C"))
-- Función que realiza la propiedad de asociatividad en una proposición lógica.
asociatividad_der :: Prop -> Prop
asociatividad_der (Var x) = Var x
asociatividad_der (Conj (Conj (Var a) (Var b)) (Var c)) = Conj (Var a) (Conj (Var b) (Var c))
asociatividad_der (Disy (Disy (Var a) (Var b)) (Var c)) = Disy (Var a) (Disy (Var b) (Var c))
asociatividad_der (Conj (Var b) (Conj (Var a) (Var c))) = Conj (Var b) (Conj (Var a) (Var c))
asociatividad_der (Disy (Var b) (Disy (Var a) (Var c))) = Disy (Var b) (Disy (Var a) (Var c))
asociatividad_der (Conj p (Conj r s)) = Conj (asociatividad_der p) (asociatividad_der (Conj s p))
asociatividad_der (Disy p (Disy r s)) = Disy (asociatividad_der p) (asociatividad_der (Disy s p))
asociatividad_der (Conj (Conj r s) p) = Conj (asociatividad_der (Conj s p)) (asociatividad_der p)
asociatividad_der (Disy (Disy r s) p) = Disy (asociatividad_der (Disy s p)) (asociatividad_der p)
asociatividad_der (Impl p q) = Impl (asociatividad_der p) (asociatividad_der q)
asociatividad_der (Syss p q) = Syss (asociatividad_der p) (asociatividad_der q)
asociatividad_der (Neg p) = Neg (asociatividad_der p)


-- Ejercicio 4
-- let e5 = Conj (Var "a") (Conj (Var "b") (Var "c"))
-- let e6 = Disy (Var "a") (Disy (Var "b") (Var "c"))
-- Función que realiza la propiedad de asociatividad en una proposición lógica.
-- Función que realiza la propiedad de asociatividad en una proposición lógica.
asociatividad_izq :: Prop -> Prop
asociatividad_izq (Var x) = Var x
asociatividad_izq (Conj (Var c) (Conj (Var a) (Var b))) = Conj (Conj (Var c) (Var a)) (Var b)
asociatividad_izq (Disy (Var c) (Disy (Var a) (Var b))) = Disy (Disy (Var c) (Var a)) (Var b)
asociatividad_izq (Conj (Conj (Var a) (Var b)) (Var c)) = Conj (Conj (Var a) (Var b)) (Var c)
asociatividad_izq (Disy (Disy (Var a) (Var b)) (Var c)) = Disy (Disy (Var a) (Var b)) (Var c)
asociatividad_izq (Conj p (Conj r s)) = Conj (asociatividad_izq p) (asociatividad_izq (Conj s p))
asociatividad_izq (Disy p (Disy r s)) = Disy (asociatividad_izq p) (asociatividad_izq (Disy s p))
asociatividad_izq (Conj (Conj r s) p) = Conj (asociatividad_izq (Conj s p)) (asociatividad_izq p)
asociatividad_izq (Disy (Disy r s) p) = Disy (asociatividad_izq (Disy s p)) (asociatividad_izq p)
asociatividad_izq (Impl p q) = Impl (asociatividad_izq p) (asociatividad_izq q)
asociatividad_izq (Syss p q) = Syss (asociatividad_izq p) (asociatividad_izq q)
asociatividad_izq (Neg p) = Neg (asociatividad_izq p)


-- Ejercicio 5
-- let e7 = (Conj (Var "A") (Var "B"))
-- Usar e2
-- Función que comnuta a las variables de una proposición lógica.
comnutatividad :: Prop -> Prop
comnutatividad (Var a) = Var a
comnutatividad (Neg a) = Neg (comnutatividad a)
comnutatividad (Conj (Var a) (Var b)) = Conj (Var b) (Var a)
comnutatividad (Disy (Var a) (Var b)) = Disy (Var b) (Var a)
comnutatividad (Conj a b) = Conj (comnutatividad b) (comnutatividad a)
comnutatividad (Disy a b) = Disy (comnutatividad b) (comnutatividad a)
comnutatividad (Impl a b) = Impl (comnutatividad a) (comnutatividad b)
comnutatividad (Syss a b) = Syss (comnutatividad a) (comnutatividad b)


-- Ejercicio 6
-- let e8 = Conj (Var "A") (Disy (Var "B") (Var "C"))
-- let e9 = Disy (Var "A") (Conj (Var "B") (Var "C"))
-- Función que realiza la distributividad en una fórmula lógica
dist :: Prop -> Prop
dist (Var a) = Var a
dist (Neg a) = Neg (dist a)
dist (Conj (Var a) (Var b))  = (Conj (Var a) (Var b))
dist (Disy (Var a) (Var b))  = (Disy (Var a) (Var b))
dist (Conj (Var a) (Disy (Var b) (Var c))) = Disy (Conj (Var a) (Var b)) (Conj (Var a) (Var c))
dist (Disy (Var a) (Conj (Var b) (Var c))) = Conj (Disy (Var a) (Var b)) (Disy (Var a) (Var c))
dist (Conj a b) = Conj (dist a) (dist b)
dist (Disy a b) = Disy (dist a) (dist b)
dist (Impl a b) = Impl (dist a) (dist b)
dist (Syss a b) = Syss (dist a) (dist b)

-- Ejercicio 7
-- let e10 = Neg (Conj (Var "A") (Var "B"))
-- let e11 = Neg (Disy (Var "A") (Var "B"))
-- let e12 = Neg (Conj (Neg (Disy (Var "a") (Var "b"))) (Neg (Disy (Neg (Var "c")) (Var "d"))))
-- Función que aplica las leyes de deMorgan a una fórmula proposicional
deMorgan :: Prop -> Prop
deMorgan (Neg (Conj a b)) = dobleNeg(Disy (Neg a) (Neg b))
deMorgan (Neg (Disy a b)) = dobleNeg(Conj (Neg a) (Neg b))
deMorgan (Neg a) = dobleNeg(Neg(deMorgan a))
deMorgan (Conj p q) = dobleNeg(Conj (deMorgan p) (deMorgan q))
deMorgan (Disy p q) = dobleNeg(Disy (deMorgan p) (deMorgan q))
deMorgan (Syss p q) = dobleNeg(Syss (deMorgan p) (deMorgan q))
deMorgan (Impl p q) = dobleNeg(Impl (deMorgan p) (deMorgan q))
deMorgan a = a
-- Ejercicio 8
-- let e13 = Impl (Var "A") (Var "B")
-- let e14 = Syss (Var "A") (Var "B")
-- Función que realiza la equivalencia lógica para eliminar la implicación y el si solo sí, dejandolo en términos de conjunción o disyunción.
equiv_op :: Prop -> Prop
equiv_op (Var a) = (Var a)
equiv_op (Neg a) = Neg(equiv_op a)
equiv_op (Conj a b) = Conj (equiv_op a) (equiv_op b)
equiv_op (Disy a b) = Disy (equiv_op a) (equiv_op b)
equiv_op (Impl a b) = Conj (Neg (equiv_op a)) (equiv_op b)
equiv_op (Syss a b) = Conj (equiv_op (Impl a b)) (equiv_op (Impl b a))

-- Ejercicio 9
-- let e15 = Neg (Neg (Conj (Neg (Neg (Var "y"))) (Var "x")))
-- Función que quita la doble negación de una fórmula proposicional
dobleNeg :: Prop -> Prop
dobleNeg (Var a) = (Var a)
dobleNeg (Neg (Neg a)) = dobleNeg a
dobleNeg (PTrue) = PTrue
dobleNeg (PFalse) = PFalse
dobleNeg (Neg p) = Neg (dobleNeg p)
dobleNeg (Conj a b) = Conj (dobleNeg a) (dobleNeg b)
dobleNeg (Disy a b) = Disy (dobleNeg a) (dobleNeg b)
dobleNeg (Impl a b) = Impl (dobleNeg a) (dobleNeg b)
dobleNeg (Syss a b) = Syss (dobleNeg a) (dobleNeg b)
-- Ejercicio 10
-- Usar e15
-- Función que cuenta los conectivos lógicos que tiene una fórmula proposicional
cuenta_conectivos :: Prop -> Int
cuenta_conectivos (Var a) = 0
cuenta_conectivos (Neg a) = 1 + cuenta_conectivos a
cuenta_conectivos (Conj a b) = 1 + cuenta_conectivos a + cuenta_conectivos b
cuenta_conectivos (Disy a b) = 1 + cuenta_conectivos a + cuenta_conectivos b
cuenta_conectivos (Impl a b) = 1 + cuenta_conectivos a + cuenta_conectivos b
cuenta_conectivos (Syss a b) = 1 + cuenta_conectivos a + cuenta_conectivos b

-- Ejercicio 11
-- let e16 = Impl (Impl (Var "p") (Var "q")) (Var "r")
-- let asignacion = [("p",0),("q",0),("r",0)]
-- let asignacion2 = [("p",1),("q",0),("r",0)]
-- let asignacion3 = [("p",0),("r",0),("s",0)]

-- función que encuentra el estado en una asignación
encuentra :: (Ord a) => a -> [(a, b)] -> b
encuentra a [(b,c)] = if(a == b) then c else error"La variable no se encontró"
encuentra a [] = error "La variable no se encuentra definida"
encuentra a (x:xs) = if(a == fst (x)) then snd(x) else encuentra a xs

-- Función que realiza la interpretación de un estado junto con su fórmula lógica.
-- Modifiqué la asginación para que en lugar de un número int tuviese un valor Prop
-- de tal forma que fuese PFalso o PVerdadero y regresar un bool para facilitar el asunto.
interpretacion :: Prop -> Asignacion -> Bool
interpretacion PTrue asignacion = True
interpretacion PFalse asignacion = False
interpretacion (Var a) estado = interpretacion (encuentra a estado) estado
interpretacion (Neg a)    estado = not(interpretacion a estado)
interpretacion (Conj x y) estado= (interpretacion x estado) && (interpretacion y estado)
interpretacion (Disy x y) estado= (interpretacion x estado) || (interpretacion y estado)
interpretacion (Impl x y) estado = interpretacion (equiv_op (Impl x y)) estado
interpretacion (Syss x y) estado = interpretacion (equiv_op (Syss x y)) estado


intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys):[y:zs | zs <- (intercala x ys)]

-- Para realizar la tabla de verdad, se tendría que
-- 1) Contar las variables de tu proposicion lógica
-- 2) Generar una lista con listas dentro: cada lista tiene el número traducido a binario 0 y 1.
-- 3) Hacer la conjunción de las interpretaciones con ese respectivo estado, para eso necesitamos una
-- función que construya estados, en donde le pasamos los 1 y 0 y los traduce a una asignación que posteriormente prueba en la interpretación.

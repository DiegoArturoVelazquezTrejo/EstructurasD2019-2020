module CreditosExtra where

data ArbolBinario a = Hoja a | Void | NodoB (ArbolBinario a) a (ArbolBinario a) deriving (Show, Eq, Ord)

aplanar :: ArbolBinario a -> [a]
aplanar Void = []
aplanar (Hoja n) = [n]
aplanar (NodoB (b) (z) (c)) = aplanar(b) ++ [z] ++ aplanar(c)




import Test.HUnit

-- Definiciones de tipos

data AB a = Nil | Bin (AB a) a (AB a) deriving Eq

instance Show a => Show (AB a) where
  show t = padAB t 0 0

-- Funciones auxiliares
recr :: b -> (a->[a] -> b -> b) -> [a] -> b
recr z f [] = z
recr z f (x:xs) = f x xs (recr z f xs) 

pad :: Int -> String
pad i = replicate i ' '

padAB :: Show a => AB a -> Int -> Int -> String
padAB = foldAB (const $ const "") (\ri x rd n base ->let l = length $ show x in pad n ++ show x ++ ri 4 (base+l) ++ "\n" ++ rd (n+4+base+l) base)

-- Crea una hoja de un árbol binario AB
abHoja :: a -> AB a
abHoja x = Bin Nil x Nil

-- Devuelve una lista con los elementos de los nodos de un árbol binario AB recorridos en profundidad de izquierda a derecha
inorder :: AB a -> [a]    
inorder = foldAB [] (\i r d -> i ++ (r:d))

-- Estructuras para tests

-- Heap (<) completo
ab1 = Bin (abHoja 4) 2 (abHoja 5)
-- Heap (<) completo
ab2 = Bin (abHoja 6) 3 (abHoja 7)
-- Heap (>) completo
ab3 = Bin (Bin (abHoja 4) 5 (abHoja 2)) 7 (Bin (abHoja 3) 6 (abHoja 1))
-- Heap (<)
ab4 = Bin ab1 1 (abHoja 3)
-- ABB completo
ab5 = Bin (Bin (abHoja 1) 2 (abHoja 3)) 4 (Bin (abHoja 5) 6 (abHoja 7))
-- Heap (<)
ab6 = Bin ab1 0 (abHoja 6)
-- ABB
ab7 = Bin (Bin (abHoja 1) 2 (abHoja 4)) 5 (abHoja 7)
-- Heap (<) infinito, probar truncando
ab8 = Bin (mapAB (*2) ab8) 1 (mapAB ((+1) . (*2)) ab8)

-- Ejercicios

--recAB :: 
recAB::b-> (b -> a -> b -> AB a -> AB a -> b)-> AB a -> b
recAB fNil fBin t = case t of
    Nil -> fNil 
    Bin t1 a t2 -> fBin (rec t1) a (rec t2) t1 t2
    where rec = recAB fNil fBin
 
--foldAB' fNil fBin = recAB fNil (\izqrec r derrec t1 t2 -> fBin (izqrec) r (derrec)) 

--foldAB 
foldAB::b->(b->a->b->b)-> AB a -> b
foldAB fNil fBin t = case t of
    Nil -> fNil 
    Bin t1 a t2 -> fBin (rec t1) a (rec t2)
    where rec = foldAB fNil fBin

mapAB :: (a -> b) -> AB a -> AB b
mapAB f = foldAB Nil (\izqrec a derrec -> Bin (izqrec) (f a) (derrec))

nilOCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
nilOCumple f a Nil = True 
nilOCumple f a (Bin t1 r t2) = f a r 

esABB :: Ord a => AB a -> Bool
esABB = (\arbol -> recr True (\x xs rec -> if (xs == []) then True else ((x < head xs) && rec)) (inorder arbol)) 

esHeap :: (a -> a -> Bool)  -> AB a -> Bool
esHeap f = recAB True (\izqrec a derrec t1 t2 -> izqrec && derrec && (nilOCumple (f) a t2) && (nilOCumple (f) a t1)) 

completo :: AB a -> Bool
completo a = (2^(altura a) - 1 == (nodos a))

altura:: AB a -> Integer 
altura = foldAB 0 (\i r d-> 1 + max i d)

nodos:: AB a -> Integer 
nodos = foldAB 0 (\i r d -> 1 + i + d)

insertarABB :: Ord a => AB a -> a -> AB a
insertarABB ab a = recAB (abHoja a) (\izqrec r derrec t1 t2 -> if (a > r) then Bin t1 r (derrec) else Bin (izqrec) r t2 ) ab

insertarHeap :: (a -> a -> Bool) -> AB a -> a -> AB a
insertarHeap f =  recAB (abHoja) (\izqrec r derrec t1 t2 -> \a -> 
  if (f a r) then 
    if elHeapEstaLleno t1 t2 then Bin t1 a (derrec r) else Bin (izqrec r) a t2  
  else 
    if elHeapEstaLleno t1 t2 then Bin t1 r (derrec a) else Bin (izqrec a) r t2)

elHeapEstaLleno :: AB a -> AB a -> Bool 
elHeapEstaLleno t1 t2 =  (completo t1) && (nodos t1) > (nodos t2)

truncar :: AB a -> Integer -> AB a
truncar = recAB (siEsNilOCeroAltura) (\izqrec r derrec t1 t2 ->  \n -> if (n > 0) then Bin (izqrec (n-1)) r (derrec (n-1)) else Nil) 
    where siEsNilOCeroAltura = const Nil
--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

testsEj1 = test [
  [1,2,4,5,7] ~=? inorder ab7,
  [1,2,3,4,5,6,7] ~=? inorder ab5
  ]
  
testsEj2 = test [
  [5,3,6,1,7] ~=? inorder (mapAB (+1) ab6)
  ]

testsEj3 = test [
   True  ~=? (nilOCumple (<) 1 (Bin Nil 2 Nil)),
   False ~=? (nilOCumple (>=) 1 (Bin Nil 2 Nil))
  ]

testsEj4 = test [
  True ~=? esABB ab5,
  False ~=? esABB ab1,
  False ~=? esABB ab6,
  True ~=? esHeap (<) ab1,
  False ~=? esHeap (>) ab1
  ]

testsEj5 = test [
  True ~=? completo ab1,
  False ~=? completo ab4
  ]


a = foldl (insertarHeap (<)) Nil [4,8,15,16,23,42]

testsEj6 = test [
  True ~=? esHeap (<) (insertarHeap (<) (insertarHeap (<) ab6 3) 1),
  True ~=? esHeap (>) (insertarHeap (>) (insertarHeap (>) ab3 10) 11),
  True ~=? esABB (insertarABB (insertarABB ab7 6) 9),
  True ~=? completo (truncar a (altura a -1)) 
  ]

testsEj7 = test [
  [8,4,12,2,10,6,14,1,9,5,13,3,11,7,15] ~=? inorder (truncar ab8 4),
  True ~=? esHeap (<) (truncar ab8 5)
  ]

import Data.List 
import System.IO 
import Data.Dynamic


 
-- 2.a  
valorAbsoluto :: Float ->  Float 
valorAbsoluto n = if n > 0 then n else (-n) 
-- 2.b
bisiesto :: Int -> Bool 
bisiesto n = mod n 4 == 0

-- 2.c
factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial(n-1)
-- 2.d
cantDivisoresPrimos :: Int->Int 
cantDivisoresPrimos n =  length [ x | x <-factores n , esPrimo x]

esPrimo:: Int -> Bool
esPrimo 1 = False
esPrimo n = null [x | x <- [2..n-1], (mod n x) == 0]

factores :: Int -> [Int] 
factores n = [x | x <- [1..n], mod n x == 0]

data Maybe a = Nothing | Just a 
data Either a b = Left a | Right b 
-- 3.a
inverso :: Float -> Prelude.Maybe Float 
inverso x = if ( x == 0) then Prelude.Nothing else (Prelude.Just (inver x))
inver :: Float -> Float
inver x = 1/x
-- 4.a
limpiar:: [Char] -> [Char] -> [Char]  
limpiar xs ys = [z | z<- ys, not (elem z xs) ]
-- 4.b
difPromedio :: [Float] -> [Float]
difPromedio xs = map (\x -> x - promedio xs) xs

promedio :: [Float] -> Float
promedio [] = 0
promedio xs = sum xs / fromIntegral (length xs)

--4.c
todosIguales :: [Int] -> Bool 
todosIguales [] = True 
todosIguales [x] = True
todosIguales (x:(y:xs)) = (x == y) && todosIguales(y:xs)
--5
data AB a = Nil | Bin (AB a) a (AB a) 

arbolNil :: AB Int 
arbolNil = Nil

arbol1 :: AB Int
arbol1 = Bin (Nil) 1 (Nil)

arbol2 :: AB Int 
arbol2 = Bin (Bin (Nil) 1 (Nil)) 2 (Bin (Nil) 1 (Nil))

arbol3 :: AB Bool 
arbol3 = Bin (Bin (Nil) False (Nil)) True (Bin (Nil) True (Nil))

vacioAB :: AB a -> Bool 
vacioAB Nil = True
vacioAB _ = False

{-
insertAB :: AB a -> a -> AB a 
insert Nil x = arbol1 
insert (Bin l a r) x
| x == a = Bin (l a r)
| x < a = Bin l a (insertAB r x) 
| x > a = Bin (insert l x) a r


deleteAB :: AB a -> a -> Ab a 
deleteAB Nil _ = Nil 
deleteAB (Bin l a r) x 
| x == a = deleteR (Bin l a r)
| x < a = Bin (deleteAB r x) a r 
| x > a = Bin l a (deleteAB r x)


deleteR :: AB a -> AB a 
deleteR (Bin Nil a r) = r 
deleteR (Bin l a Nil) = l
deleteR (Bin l a r) = (Bin l a1 r) where a1 = leftistElement r 

leftistElement :: AB a -> a 
leftistElement(Bin Nil a _) = a 
leftistElement(Bin l _ _) = leftistElement l 

negacionAB :: AB Bool -> AB Bool 
negacionAB Nil = Nil
negacionAB (Bin left a right) = (Bin left a right)
--}


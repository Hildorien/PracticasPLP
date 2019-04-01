{- Comentarios -}
-- Comentarios 
-- :l archivo.hs para cargar archivo 
-- :r para correr archivo.hs 
-- :t funcion muestra la aridad de la funcion

import Data.List 
import System.IO 


getListItems :: [Int] -> String 

getListItems [] = "Your list is Empty" 
getListItems (x:[]) = "Your list starts with " ++ show x 
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y 
getListItems (x:xs) = "Your list starts with " ++ show x ++ " and the rest are " ++ show xs


times4 :: Int -> Int 
times4 x = x * 4 

listTimes4 = map times4 [1,2,3,4]

multBy4 :: [Int] -> [Int]

multBy4 [] = [] 
multBy4  (x:xs) = times4 x : multBy4 xs


doMult :: (Int -> Int) -> Int 

doMult func = func 3

num3Times4 = doMult times4


getAddFunc:: Int -> (Int -> Int) 

getAddFunc x y = x + y 

adds3 = getAddFunc 3 

fourPlus3 = adds3 4

dbl1To10 = map (\x -> x*2) [1..10]




















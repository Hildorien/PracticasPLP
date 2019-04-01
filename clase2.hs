-- Clase 2 --
import System.IO 
import Data.Dynamic

pares::[(Int,Int)]
pares = [(x,z-x) | z <-[0..], x <- [0..z]]

triplas :: [(Int,Int,Int)]
triplas = [(x,y,w-(x+y)) | w <-[0..], y <- [0..w], x<-[0..w-y]]

listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n = [x:xs | x<-[1..n], xs <- listasQueSuman(n-x)]

listasPositivas :: [[Int]]
listasPositivas = [ xs | n <- [0..], xs <- listasQueSuman(n)]

negar :: [[Char]] -> [[Char]] 
negar = map (\ x -> "in" ++ x)

sinVacias :: [[Int]] -> [[Int]]
sinVacias = filter (\xs -> not(null xs))

cumpleTodos::(Int -> Bool) -> [Int] -> Bool 
cumpleTodos p = foldr (\x rec-> (p x) && rec) True

concat':: [[a]] -> [a]
concat' = foldr (\xs rec -> xs ++ rec) []

longitud :: [a] -> Int
longitud = foldr (\x rec -> 1 + rec) 0

producto :: [Int] -> Int 
producto = foldr (*) 1

map'::(a -> b) -> [a] -> [b]
map' f = foldr (\x rec -> f x : rec) []

filter'::(a->Bool) -> [a] -> [a]
filter' p = foldr (\x rec -> if (p x) then x:rec else rec) []

producto' :: [Int] -> Int 
producto' = foldl (\acum x -> x * acum) 1

reverso :: [a] -> [a]
reverso = foldl (\acum x -> x : acum) []

last' :: [a] -> a
last' = foldl1 (\acum x -> x)

maximum' :: Ord a =>  [a] -> a
maximum' = foldr1 (\x rec -> if x > rec then x else rec)

recr :: b -> (a->[a] -> b -> b) -> [a] -> b
recr z f [] = z
recr z f (x:xs) = f x xs (recr z f xs)

insertarOrdenado :: Ord a => a -> [a] -> [a] 
insertarOrdenado e = recr [e] (\x xs rec -> if e < x then e:x:xs else x:rec)

pertenece :: Eq a => a -> [a] -> Bool 
pertenece e = foldr (\x rec -> (x == e) || rec) False

take' :: Int -> [a] -> [a] 
take' n = flip takeaux

takeaux :: [Int] -> Int -> [Int]
takeaux = foldr (\x rec -> \n if (n>0) then x:rec(n-1) else []) (const [])

take'' :: Int -> [a] -> [a] 
take'' n = foldl (\acum x -> if (length acum < n) then acum++[x] else acum) []


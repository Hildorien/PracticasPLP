import Data.List 
import System.IO 
import Data.Dynamic

--1
max2 (x,y) | x >= y = x 
           | otherwise = y
normaVectorial (x,y) = sqrt(x^2 + y^2)
substract = flip (-)
predecesor = substract 1
evaluarEnCero = \f -> f 0
dosVeces = \f -> f.f
flipAll = map flip
flipRaro = flip flip

max2C :: Float -> (Float -> Float)
max2C x = (\y -> if x > y then x else y)

normaVectorialC :: Float -> (Float -> Float)
normaVectorialC x = (sqrt . (+) (x^2) . flip (^) 2)
--4
esPitagorica :: Integer -> Integer -> Integer -> Bool 
esPitagorica a b c = a*a + b*b == c*c

pitagoricas :: Integer -> [(Integer, Integer, Integer)]
pitagoricas n = [(a, b, c) | c <- [1..n], 
                             b <- [1..c], 
                             a <- [1..b],  
                             esPitagorica a b c]
pitagoricasInf :: [(Integer, Integer, Integer)]
pitagoricasInf = [(a, b, c) |   c <- [1..], 
                             b <- [1..c], 
                             a <- [1..b],  
                             esPitagorica a b c]

                           
esPrimo:: Int -> Bool
esPrimo 1 = False
esPrimo n = null [x | x <- [2..n-1], (mod n x) == 0]
--5
primos1000 = take 1000 [ p | p <- [1..] , esPrimo p]

--6
partir :: [Integer] -> [([Integer], [Integer])]
partir l = [ (xs,ys) | i<-[0..(length l)],
                       xs<-[take i l],
                       ys<-[drop i l], 
                       (xs ++ ys == l)]

sacarDuplicados :: [Integer] -> [Integer]
sacarDuplicados = map head . group . sort

{-
sacarDuplicados [1,2,1,3,2,4] 
I. Ordeno [1,1,2,2,3,4]
II. Group [[1,1],[2,2],[3],[4]]
III. Me quedo con la cabeza de cada sublista [1,2,3,4] (map head)
-}


-- [1,2,3] = [[3], [1,2],[2,1],[1,1,1]]
--7
listasQueSuman :: Integer -> [[Integer]] 
listasQueSuman 1 = [[1]]
listasQueSuman n = map suma1 (listasQueSuman(n-1)) ++ map agrega1 (listasQueSuman(n-1))

suma1 :: [Integer] -> [Integer]
suma1 (x:xs) = (x+1):xs

agrega1 :: [Integer] -> [Integer]
agrega1 xs = 1:xs


listaDeUnNumero :: Integer -> [Integer]
listaDeUnNumero n = [x | x <- [1..n]]

--8
todosLosEnterosFinitos :: [[Integer]]
todosLosEnterosFinitos = [xs | i<-[1..], xs <-[listaDeUnNumero i]]

-- 10 
-- I 
sum'::[Integer] -> Integer 
sum' = foldr (+) 0

elem'::Eq a => a -> [a] -> Bool 
elem' e = foldr (\x rec -> (x == e) || rec) False

concat':: [[a]] -> [a]
concat' = foldr (\xs rec -> xs ++ rec) [] 

map'::(a -> b) -> [a] -> [b]
map' f = foldr (\x rec -> f x : rec) []

filter'::(a->Bool) -> [a] -> [a]
filter' p = foldr (\x rec -> if (p x) then x:rec else rec) [] 

-- II 
mejorSegun:: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if (f x rec) then x else rec)

-- III 
sumaAlt::[Integer] -> Integer
sumaAlt = foldr (\x rec->  x - rec) 0

rest ::[Integer] -> Integer 
rest = foldr(\x rec -> (-1)*x + rec) 0

obtenerIndices:: [a] -> [(a,Int)]
obtenerIndices xs = zip xs [0..length xs]

tieneIndicePar:: Eq a => a -> [(a,Int)] -> Bool 
tieneIndicePar e xs = odd (head [snd p| p <-xs, fst p == e])

--IV 
sumaAlt'::[Integer] -> Integer
sumaAlt' = foldl (\acum x -> x - acum) 0

-- V 
permutacionesRec:: Eq a => [a] -> [[a]] 
permutacionesRec [] = [[]]
permutacionesRec l = [a:x | a<-l,x<-(permutacionesRec $ filter (\x -> x /= a) l)]

--permutaciones:: Eq a => [a] -> [[a]]
--permutaciones = foldr (\p rec -> concatMap (\x-> x:rec(filter(\a -> a /= x) rec)) rec) [[]]

-- 11 
-- I 
partes::[a] -> [[a]]
partes = foldr (\x rec -> rec ++ map (x:) rec) [[]]

partes2::[a] -> [[a]]
partes2 = foldl (\acum x -> acum ++ map(x:) acum)[[]]

-- II 
prefijos::[a] -> [[a]]
prefijos = foldr (\x rec -> [] : map (x:) rec) [[]] 

prefijos'::[a] -> [[a]]
prefijos' [] = [[]]
prefijos' (x:xs) = [] : map (x:) (prefijos xs)

-- III 
sublistas::[a] -> [[a]]
sublistas xs = [] : (concatMap(\x -> tail(prefijos x)) (sufijos xs))

sufijos::[a] -> [[a]] 
sufijos = foldr (\x rec -> (x:(head rec)):rec) [[]]

concatMap2::(a -> [b]) -> [a] -> [b]
concatMap2 f = foldr (\x rec -> f x ++ rec) [] 

-- 12 
recr :: b -> (a->[a] -> b -> b) -> [a] -> b
recr z f [] = z
recr z f (x:xs) = f x xs (recr z f xs) 

sacarUna:: Eq a => a -> [a] -> [a]
sacarUna e = recr [] (\x xs rec -> if e == x then xs else x:rec)


-- 13 
-- I
genListaRec:: (Eq a, Num a) => a->(a->a)->Integer->[a]
genListaRec a f 0 = []
genListaRec a f b = f a : (genListaRec (f a) f (b-1)) 

generaLista a f b = a: map f [a..(a-1)+(b-1)]

generaLista' a b = [a..(a-1)+(b-1)]

-- II 
desdeHasta a b = genListaRec a succ (b-a)

-- 14
mapPares :: (a-> (b-> c)) -> [(a,b)] -> [c] 
mapPares f = foldr (\x rec -> f (fst x) (snd x) : rec) []
{--mapPares f xs = zipWith g a b where
 g = (\x y -> (x,y))
 a = (map f (dameFst xs))
 b = (map f (dameSnd xs))--}

dameFst:: [(a,a)] -> [a]
dameFst xs = [fst x | x <- xs]

dameSnd::[(a,a)] -> [a]
dameSnd xs = [snd x | x <- xs]

mapParesRec :: (a-> (a-> c)) -> [(a,a)] -> [c]
mapParesRec f [] = []
mapParesRec f (x:xs) = f (fst x) (snd x) : mapParesRec f xs

-- II
armarParesRec::[a]->[b]->[(a,b)]
armarParesRec [] [] = [] 
armarParesRec _ [] = [] 
armarParesRec [] _ = []
armarParesRec (x:xs) (y:ys) = (x,y) : armarPares xs ys

armarPares:: [a] -> [b] -> [(a,b)]
armarPares = foldr (\x rec -> \ys -> if(length ys == 0) then ([]) else ((x,head ys): rec(tail ys))) (const[]) 

-- III 
mapDobleRec:: (a -> b -> c) -> [a] -> [b] -> [c] 
mapDobleRec f [] [] = [] 
mapDobleRec f _ [] = [] 
mapDobleRec f [] _ = [] 
mapDobleRec f (x:xs) (y:ys) = f x y : mapDobleRec f xs ys 

mapDoble:: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f (armarPares xs ys)

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = foldr (\x rec -> \ys-> (zipWith (+) x (head ys)): rec (tail ys)) (const [])

trasponerRec :: [[Int]] -> [[Int]]
trasponerRec ([]:_) = []
trasponerRec xs =  (map head xs) : trasponerRec (map tail xs)

trasponer :: [[Int]] -> [[Int]]
trasponer l = [ map (!! i) l | i<-[0..(length (head l) - 1)] ]

-- 16 
-- I 

-- 17 

data Nat = Zero | Succ Nat deriving (Show,Eq)

cinco :: Nat
cinco = Succ(Succ(Succ(Succ(Succ Zero))))

dos :: Nat
dos = Succ(Succ Zero)

uno :: Nat
uno = Succ Zero

foldNat::(b -> b) -> b -> Nat -> b 
foldNat s z Zero = z
foldNat s z (Succ n) = s (foldNat s z n)

sumaRec :: Nat -> Nat -> Nat
sumaRec x Zero = x
sumaRec x (Succ y) = Succ(sumaRec x y)

suma :: Nat -> Nat -> Nat
suma m  = foldNat Succ m 

productoRec:: Nat -> Nat -> Nat
productoRec x Zero = Zero 
productoRec x (Succ y) = suma x (productoRec y x)

producto :: Nat -> Nat -> Nat
producto n = foldNat (\x -> suma x n) Zero


potenciaRec :: Nat -> Nat -> Nat 
potenciaRec x Zero = Succ(Zero)
potenciaRec x (Succ y) = producto x (producto x y)

potencia :: Nat -> Nat -> Nat 
potencia a = foldNat (\x-> producto a x ) (Succ(Zero))


foldNatConInt :: (a -> a) -> a -> Integer -> a 
foldNatConInt s z 0 = z 
foldNatConInt s z n = s (foldNatConInt s z (n-1))

potenciaConInt :: Integer -> Integer -> Integer
potenciaConInt a = foldNatConInt (\x -> a*x) 1

data Polinomio a = X 
                 | Cte a 
                 | Suma (Polinomio a) (Polinomio a)
                 | Prod (Polinomio a) (Polinomio a)

foldPoli:: Num a => b -> (a->b) -> (b->b->b) -> (b->b->b) -> Polinomio a -> b 
foldPoli casoX casoCte casoSuma casoProd p = case p of
                                                  X -> casoX
                                                  Cte a -> casoCte a 
                                                  Suma a b -> casoSuma (rec a) (rec b)
                                                  Prod a b -> casoProd (rec a) (rec b)
                                                  where rec = foldPoli casoX casoCte casoSuma casoProd

evaluar:: Num a => a-> Polinomio a -> a 
evaluar a = foldPoli a (id) (+) (*) 

parabola::Num a=> Polinomio a 
parabola = Suma (Suma (Prod X X) X) (Cte 1)







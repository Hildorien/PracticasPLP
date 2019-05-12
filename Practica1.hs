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

-- 9
type DivideConquer a b = (a -> Bool)
 -> (a -> b)
 -> (a -> [a])
 -> ([b] -> b)
 -> a 
 -> b 

-- I 
dc :: DivideConquer a b 
dc trivial solve split combine x = if trivial x then solve x else combine (map dc1 (split x)) 
 where dc1 = dc trivial solve split combine

--II 
mergeSort:: Ord a => [a] -> [a]
mergeSort = dc (\l-> length l <= 1) id partirALaMitad (\[xs,ys]-> merge xs ys)

partirALaMitad :: [a]->[[a]] 
partirALaMitad xs = [take i xs, drop i xs] where i = div (length xs) 2

merge:: Ord a => [a]->[a]->[a]
merge = foldr (\x rec ->(filter (<= x) rec) ++ [x] ++ (filter (>x) rec))

-- III
mapDC :: (a -> b) -> [a] -> [b]
mapDC f = dc (\l-> length l <= 1) (\xs-> if length xs == 0 then [] else [f(head xs)]) partirALaMitad concat 

filterDC :: (a -> Bool) -> [a] -> [a]
filterDC p = dc (\l-> length l <= 1) (\xs -> if (length xs == 0 || p(head xs)) then [] else xs ) partirALaMitad concat

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

permutaciones:: [Integer] -> [[Integer]]
permutaciones = foldr (\x rec -> concatMap (agregarEnTodasLasPosiciones x) rec ) [[]] where agregarEnTodasLasPosiciones j js = [(fst h) ++ [j] ++ (snd h) | h <- (partir js)]


agregarEnTodasLasPos j js = [(fst h) ++ [j] ++ (snd h) | h <- (partir js)]
--permutaciones:: Eq a => [a] -> [[a]]
--permutaciones = foldr (\p rec -> concatMap (\x-> x:rec(filter(\a -> a /= x) rec)) rec) [[]]

-- 11 
-- I 
partes::[a] -> [[a]]
partes = foldr (\x rec -> rec ++ map (x:) rec) [[]]


-- II 
prefijos::[a] -> [[a]]
prefijos = foldr (\x rec -> [] : map (x:) rec) [[]] 

prefijos'::[a] -> [[a]]
prefijos' [] = [[]]
prefijos' (x:xs) = [] : map (x:) (prefijos xs)

-- III 
sublistas::[a] -> [[a]]
sublistas xs = [] : (concatMap(\x -> tail(prefijos x)) (sufijos xs))

sublistas'::[a] -> [[a]]
sublistas' xs = [[]] ++ [take j (drop i xs) | i<-[0..length xs], j <-[1..length xs -i]]

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

sacarUna':: Eq a => a -> [a] -> [a]
sacarUna' e = foldr (\x rec -> if e == x then rec else x:rec) []

-- 13 
-- I
genListaRec:: (Eq a, Num a) => a->(a->a)->Integer->[a]
genListaRec a f 0 = []
genListaRec a f b = a : (genListaRec (f a) f (b-1)) 

generaLista a f b = a: map f [a..(a-1)+(b-1)]

generaLista' a b = [a..(a-1)+(b-1)]

genLista::(Eq a, Num a) => a->(a->a)->Integer->[a]
genLista a f b = foldr(\x rec -> rec ++ [f(head(reverse rec))]) [a] [1..b-1]


-- II 
desdeHasta a b = genListaRec a succ (b+1-a)
desdeHasta' a b = generaLista a succ (b+1-a)

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

--15
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = foldr (\x rec -> \ys-> (zipWith (+) x (head ys)): rec (tail ys)) (const [])

trasponerRec :: [[Int]] -> [[Int]]
trasponerRec ([]:_) = []
trasponerRec xs =  (map head xs) : trasponerRec (map tail xs)

trasponer :: [[Int]] -> [[Int]]
trasponer l = [ map (!! i) l | i<-[0..(length (head l) - 1)] ]

trasponer' :: [[Int]] -> [[Int]]
trasponer' xss = foldr (zipWith (:)) [ [] | i <-[1..length(head xss)]] xss

-- 16 
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs
 | stop xs = init xs 
 | otherwise = generateFrom stop next (xs ++ [next xs])
--I
generateBase:: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop base next = generate stop (\xs-> if (length xs == 0) then base else next (last xs))
-- II
factoriales :: Int -> [Int]
factoriales n = generate ((==n).length) (\l -> if null l then 1 else (last l)*(length l))
-- III
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase ((==n).length) x (\u-> f u)
--iterateN n f x = generate ((==n).length) (\l -> if null l then x else f(last l)) 
-- IV 
generateFrom' :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom' stop next = last.(takeWhile (not.stop)).(iterate (\ys -> ys ++ [next ys]))

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

-- 18
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

-- 19 
type Conj a = (a -> Bool)

--I
vacio:: Conj a
vacio = const False

agregar::Eq a => a -> Conj a -> Conj a 
agregar e c = (\elem -> (c elem) || (elem == e ))
--II
union:: Conj a -> Conj a -> Conj a 
union c d = (\e -> c e || d e)

interseccion:: Conj a -> Conj a -> Conj a 
interseccion c d = (\e -> c e && d e)

-- III 
conjuntoInfinito :: Conj a
conjuntoInfinito = const True

-- IV 
singleton:: Eq a => a -> Conj a 
singleton = (==)

-- V 

-- 20 
type MatrizInfinita a = Integer -> Integer -> a 

identidad:: (Num a ,Eq a) => MatrizInfinita a
identidad = \i j -> if (i==j) then 1 else 0 

cantor = \x y -> (x+y) * (x+y+1)`div`2+y

pares = \x y -> (x,y)
--I
fila::Integer -> MatrizInfinita a -> [a] 
fila f a = [a f j | j<-[0..]]

columna::Integer -> MatrizInfinita a -> [a]
columna c a = [a i c | i<-[0..]]

--II 
trasponer2:: MatrizInfinita a -> MatrizInfinita a 
trasponer2  a = \i j -> a j i

-- III 
mapMatriz:: (a -> b) -> MatrizInfinita a -> MatrizInfinita b 
mapMatriz f a = (\i j-> f (a i j))

filterMatriz:: (a->Bool) -> MatrizInfinita a -> [a]
filterMatriz p a = [a x (z-x) | z<-[0..], x<-[0..z], p (a x (z-x))] 

zipWithMatriz::(a->b->c)->MatrizInfinita a -> MatrizInfinita b -> MatrizInfinita c 
zipWithMatriz f a b = \i j -> f (a i j) (b i j) 

todosLosParesN p = [(x,z-x) | z<-[0..], x<-[0..z], p z x]
-- IV 
sumaMatriz :: Num a => MatrizInfinita a-> MatrizInfinita a -> MatrizInfinita a
sumaMatriz = zipWithMatriz (+)

zipMatriz :: MatrizInfinita a -> MatrizInfinita b -> MatrizInfinita (a,b)
zipMatriz = zipWithMatriz (\x y -> (x,y))

-- 21 
data AHD tInterno tHoja = Hoja tHoja | Rama tInterno (AHD tInterno tHoja) | Bin' (AHD tInterno tHoja) tInterno (AHD tInterno tHoja)

arbolCharString = Bin' (Hoja "hola") 'b' (Rama 'c' (Hoja "chau"))

arbolIntBool = Rama 1 (Bin'(Hoja True)(-2)(Hoja False))
--I
foldAHD::(b->c)->(a->c->c)->(c->a->c->c)->AHD a b->c
foldAHD fHoja fRama fBin t = case t of
 Hoja h -> fHoja h
 Rama r t -> fRama r (rec t)
 Bin' t1 r t2 -> fBin (rec t1) r (rec t2)
 where rec = foldAHD fHoja fRama fBin

--II
mapAHD:: (a->b) -> (c->d) -> AHD a c -> AHD b d
mapAHD fNodos gHojas = foldAHD (\h-> Hoja (gHojas h) ) (\i rrec -> Rama (fNodos i) rrec) (\izqrec r derrec -> Bin' izqrec (fNodos r) derrec) 

-- 22 
data AB a = Nil | Bin (AB a) a (AB a) deriving (Eq, Show)

arbol1 = (Bin (Bin (Nil) 1 (Nil) ) 1 (Bin (Nil) 3 (Nil)))

arbol2 = (Bin (Bin (Nil) 3 (Nil) ) 2 (Bin (Nil) 1 (Nil)))
--I 
foldAB::b->(b->a->b->b)-> AB a -> b
foldAB fNil fBin t = case t of
    Nil -> fNil 
    Bin t1 a t2 -> fBin (rec t1) a (rec t2)
    where rec = foldAB fNil fBin

-- II 
esNil::AB a -> Bool 
esNil a = case a of
   Nil -> True 
   Bin i a d -> False

altura:: AB a -> Integer 
altura = foldAB 0 (\i r d-> 1 + max i d)

hojas:: AB a -> Integer 
hojas = foldAB 1 (\i r d -> i+d)

nodos:: AB a -> Integer 
nodos = foldAB 0 (\i r d -> 1 + i + d)

ramas:: AB a -> [[a]] 
ramas = foldAB [[]] (\i r d-> (map (r:) i) ++ (map (r:) d))


espejoRec:: AB a -> AB a 
espejoRec Nil = Nil 
espejoRec (Bin t1 x t2) = Bin (espejoRec t2) x (espejoRec t1) 

espejo:: AB a -> AB a 
espejo = foldAB (Nil) (\i r d -> Bin d r i)

-- III 
root:: AB a -> a 
root (Bin i x d) = x 

izqAB :: AB a -> AB a 
izqAB (Bin i x d) = i

derAB :: AB a -> AB a 
derAB (Bin i x d) = d 

mismaEstructura:: AB a -> AB b -> Bool 
mismaEstructura t1 t2 = nodos t1 == nodos t2 && 
 hojas t1 == hojas t2 && 
 altura t1 == altura t2 && 
 and (zipWith (\xs ys -> length xs == length ys) (ramas t1) (ramas t2))
--mismaEstructura = foldAB(\izqrec r derrec -> \arbol -> if (esNil arbol) then False else ( root arbol == r && izqrec (izqAB arbol) && derrec (derAB arbol))) 

-- 23 
data RoseTree a = Rose a [RoseTree a]
-- I
foldRose::(a->[b]->b)-> RoseTree a -> b 
foldRose f (Rose x hs) = f x (map (foldRose f) hs)


rosetree1 = Rose 1 [Rose 2 [Rose 3 [], Rose 4 []], Rose 5 []] 

hojasRT::(Eq a) => RoseTree a -> [a]
hojasRT = foldRose (\x hs -> if (hs == []) then [x] else concat hs) 

distanciaRT::(Eq a) => RoseTree a -> [Int]
distanciaRT = foldRose(\x hs-> if(not(hs == [])) then (map (+1) (concat hs)) else [0])

alturaRT :: (Eq a) => RoseTree a -> Int 
alturaRT = foldRose(\x hs -> if (hs == []) then 1 else 1+ (maximum hs) )


fix:: (a->a)->a 
fix f = let {x = f x} in x

prueba = fix (\y-> y+1)
prueba2 = fix (1:)



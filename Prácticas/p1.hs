-- Ejercicio 3
sumaf :: [Int] -> Int
sumaf = foldr (+) 0

elemf :: Int -> [Int] -> Bool
elemf a = foldr (\x rec -> x == a || rec) False

concatf :: [a] -> [a] -> [a]
concatf a b = foldr (:) b a 

filterf :: (a -> Bool) -> [a] -> [a]
filterf p = foldr (\x rec -> if (p x) then x : rec else rec) []

mapf :: (a -> b) -> [a] -> [b]
mapf f = foldr (\x rec -> (f x) : rec) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if (f x rec) then x else rec) 

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\acc x -> acc ++ (if null acc then [x] else [x + last acc])) []

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x acc -> x - acc) 0

sumaAltInv :: Num a => [a] -> a
sumaAltInv = foldl (\acc x -> x - acc) 0

-- Ejercicio 5
entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec ys -> if (null ys) then (x : rec []) else (x : head ys : rec (tail ys))) id

-- Ejercicio 6
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if x == e then xs else x : rec) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs rec -> if e < x then (e : x : xs) else x : rec) [e]

-- Ejercicio 7
genLista :: a -> (a -> a) -> Int -> [a]
genLista a f n = take n $ lista f a

lista :: (a -> a) -> a -> [a]
lista f x = x : (lista f $ f x)

desdeHasta :: Int -> Int -> [Int]
desdeHasta x y = drop x (genLista 0 (+1) $ y + 1)

-- Ejercicio 8

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\tup rec -> uncurry f tup : rec) []

armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = \xs ys -> mapPares f (armarPares xs ys)

-- Ejercicio 10
foldNat :: (Int -> b -> b) -> b -> Int -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n - 1))

potencia :: Int -> Int -> Int
potencia base = foldNat (\_ acc -> base * acc) 1  

-- Ejercicio 11
data Polinomio a = X 
                 | Cte a 
                 | Suma (Polinomio a) (Polinomio a) 
                 | Prod (Polinomio a) (Polinomio a)

foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli fX fCte fSuma fProd X = fX
foldPoli fX fCte fSuma fProd (Cte a) = fCte a
foldPoli fX fCte fSuma fProd (Suma p1 p2) = fSuma (rec p1) (rec p2)
    where rec = foldPoli fX fCte fSuma fProd
foldPoli fX fCte fSuma fProd (Prod p1 p2) = fProd (rec p1) (rec p2)
    where rec = foldPoli fX fCte fSuma fProd


evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPoli n id (+) (*)

-- polinomioEjemplo = Suma (Prod (Cte 2) (Prod X X)) (Suma (Prod (Cte 3) X) (Cte 1))
--                     2x^2 + 3x + 1 

-- Ejercicio 12
data AB a = Nil | Bin (AB a) a (AB a)

abEj :: AB Int
abEj = Bin (Bin Nil 3 (Bin Nil 4 Nil)) 5 (Bin Nil 8 (Bin Nil 9 Nil))

foldAB :: b -> (a -> b -> b -> b) -> AB a -> b
foldAB cNil cArbol t = case t of
        Nil -> cNil
        Bin i r d -> cArbol r (rec i) (rec d)
    where rec = foldAB cNil cArbol

recAB :: b -> (a -> AB a -> AB a -> b -> b -> b) -> AB a -> b
recAB cNil cArbol t = case t of
        Nil -> cNil
        Bin i r d -> cArbol r i d (rec i) (rec d)
    where rec = recAB cNil cArbol

esNil :: AB a -> Bool 
esNil Nil = True 
esNil _ = False

altura :: AB a -> Int
altura = foldAB 0 (\r recI recD -> 1 + (max recI recD))

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\r recI recD -> 1 + recI + recD)

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f (Bin i r d) = foldAB r (\r recI recD -> if (f r recI && f r recD) then r else (if (f recI r && f recI recD) then recI else recD)) (Bin i r d)

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\r i d recI recD -> mayor i r && menor d r && recI && recD)
    where   mayor Nil _ = True 
            mayor (Bin _ x _) y = x <= y 
            menor Nil _ = True 
            menor (Bin _ x _) y = y <= x

-- Ejercicio 14
data AIH a = Hoja a | Binh (AIH a) (AIH a)

aihEj = Binh (Binh (Hoja 3) (Hoja 4)) (Hoja 2)

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH cHoja cArbol t = case t of
        Hoja c -> cHoja c
        Binh i d -> cArbol (rec i) (rec d)
    where rec = foldAIH cHoja cArbol

alturaAIH :: AIH a -> Int
alturaAIH = foldAIH (const 1) (\i d -> 1 + max i d)

tamañoAIH :: AIH a -> Int
tamañoAIH = foldAIH (const 1) (\i d -> i + d)

-- Ejercicio 15
data RoseTree a = Rose a [RoseTree a]

rtEj = Rose 3 [Rose 4 [Rose 8 []], Rose 5 [] , Rose 9 [Rose 1 [], Rose 2 [], Rose 6 []]]

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose n hijos) = f n (map rec hijos)
    where rec = foldRose f

hojasRT :: RoseTree a -> [a] 
hojasRT = foldRose (\h rec -> if null rec then [h] else concat rec)

distanciasRT :: RoseTree a -> [(a, Int)]
distanciasRT = foldRose (\h rec -> if null rec then (h,0):(concat rec) else map (\(a,b) -> (a, b + 1)) (concat rec))
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
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x,y) : (armarPares xs ys)
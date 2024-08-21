
valorAbsoluto :: Float -> Float
valorAbsoluto x = if x >= 0 then x else x * (-1)
-- valorAbsoluto x = max x (x * (-1))

bisiesto :: Int -> Bool
bisiesto n = mod n 4 == 0

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


esPrimoAux :: Int -> Int -> Bool
esPrimoAux 1 _ = True
esPrimoAux i n = not (mod n i == 0) && esPrimoAux (i - 1) n

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux (n - 1) n

cantPrimos :: Int -> Int -> Int
cantPrimos _ 1  = 0
cantPrimos i n = if (esPrimo i) && (mod n i == 0) 
                 then 1 + cantPrimos (div n i) (div n i)
                 else cantPrimos (i - 1) n

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = cantPrimos n n


inverso :: Float -> Maybe Float 
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right y) = if y then 1 else 0


limpiar :: String -> String -> String
limpiar _ [] = []
limpiar a b = if (elem (head b) a)
              then limpiar a (tail b)
              else (head b) : (limpiar a (tail b))

difPromedio :: [Float] -> [Float]
difPromedio a = map (\x -> x - sum a / fromIntegral (length a)) a

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:xs) = if tail xs == [] then True else x == head xs && todosIguales xs


data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq nodo der) = Bin (negacionAB izq) (not nodo) (negacionAB der) 

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq nodo der) = nodo * (productoAB izq) * (productoAB der)


merge :: [Int] -> [Int] -> [Int]
merge a [] = a
merge [] b = b
merge a b = if (head a < head b) 
            then (head a) : merge (tail a) b
            else (head b) : merge a (tail b)

mergeSort :: [Int] -> [Int]
mergeSort (x:[]) = [x]
mergeSort a = merge (mergeSort (take mitad a)) (mergeSort (drop mitad a)) 
              where mitad = div (length a) 2


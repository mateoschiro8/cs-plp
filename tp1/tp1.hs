module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit
import Data.Maybe

--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.


-- Ejemplos para tests
rtConHijos :: RoseTree Int
rtConHijos = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 

rtSinHijos :: RoseTree Int
rtSinHijos = Rose 3 []

rtConUnHijo :: RoseTree Int
rtConUnHijo = Rose 4 [Rose 1 []]

atConHijos :: AT Int
atConHijos = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)

atSinHijos :: AT Int
atSinHijos = Tern 3 (Nil) (Nil) (Nil) 

atNil :: AT Int
atNil = Nil

atEj4 :: AT Int
atEj4 = Tern 16 (Tern  1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil))
                (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil))
                (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))

trieBool :: Trie Bool
trieBool = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]

trieRaizVacia :: Trie Int
trieRaizVacia = TrieNodo Nothing [('a', TrieNodo (Just 1) [])]

trieSinHijos :: Trie Int
trieSinHijos = TrieNodo (Just 4) []

trieEj6 :: Trie Bool
trieEj6 = TrieNodo Nothing 
  [('a', TrieNodo (Just True) []),
    ('b', TrieNodo Nothing
      [('a', TrieNodo (Just True)
        [('d', TrieNodo Nothing [])])
      ]),
    ('c', TrieNodo (Just True) [])
  ]

trieEj6ConNum :: Trie Int
trieEj6ConNum = TrieNodo Nothing 
  [('a', TrieNodo (Just 5) []),
    ('b', TrieNodo Nothing
      [('a', TrieNodo (Just 2)
        [('d', TrieNodo Nothing [])])
      ]),
    ('c', TrieNodo (Just 3) [])
  ]

-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right
        
        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where 
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
-- type Procesador a b = a -> [b]

procVacio :: Procesador a b
procVacio = const []

procId :: Procesador a a
procId = flip (:) []

procCola :: Procesador [a] a
procCola = drop 1

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose _ hijos) = hijos

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAT (Tern _ i m d) = [i, m, d]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo r _) = [r]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo _ xs) = xs


--Ejercicio 2

{-- 
Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) 

RoseTrees
data RoseTree a = Rose a [RoseTree a] 

Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)]
--}

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT cNil cNodo at = case at of
    Nil -> cNil
    Tern r i m d -> cNodo r (rec i) (rec m) (rec d)
      where rec = foldAT cNil cNodo

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose r hijos) = f r (map rec hijos)
    where rec = foldRose f 

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie f (TrieNodo r hijos) = f r (map rec hijos)
    where rec (c, t) = (c, foldTrie f t)


--Ejercicio 3 
unoxuno :: Procesador [a] [a]
unoxuno = map (:[])

sufijos :: Procesador [a] [a]
sufijos = foldr (\x rec -> (x : head rec) : rec) [[]]

--Ejercicio 4

preorder :: AT a -> [a]
preorder = foldAT [] (\r recI recM recD -> [r] ++ recI ++ recM ++ recD)

inorder :: AT a -> [a]
inorder = foldAT [] (\r recI recM recD -> recI ++ recM ++ [r] ++ recD)

postorder :: AT a -> [a]
postorder = foldAT [] (\r recI recM recD -> recI ++ recM ++ recD ++ [r])


--Ejercicio 5

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\r recHijos -> r : concat recHijos)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\r recHijos -> if null recHijos then [r] else concat recHijos)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\r recHijos -> if null recHijos then [[r]] else map (r:) (concat recHijos))

--Ejercicio 6

caminos :: Trie a -> [[Char]]
caminos = foldTrie (\r recHijos -> []:(concatMap (\(c, caminosHijos) -> (map (c:) caminosHijos)) recHijos))

--Ejercicio 7

palabras :: Trie a -> [[Char]]
palabras = foldTrie (\r recHijos -> 
        let resultadosHijos = (concatMap (\(c, caminosHijos) -> (map (c:) caminosHijos)) recHijos) in
        if isJust r then []:resultadosHijos else resultadosHijos) 

--Ejercicio 8

-- type Procesador a b = a -> [b]

-- 8.a)
ifProc :: (a -> Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc f proc1 proc2 = (\e -> if f e then proc1 e else proc2 e)

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) proc1 proc2 = (\e -> proc1 e ++ proc2 e)

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) proc1 proc2 = (\e -> concatMap proc1 (proc2 e))

--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

testsEj1 = test [
  procVacio [1,2,3]             
    ~=? ([] :: [Int])
  ,
  procVacio ['a','b']             
    ~=? ([] :: [Int])                                                          
  ,
  procVacio rtConHijos
    ~=? ([] :: [Int])
  ,
  procId [1,2,3]             
    ~=? [[1,2,3]]
  ,
  procId ['a','b']             
    ~=? [['a','b']]                                                          
  ,
  procId rtConHijos
    ~=? [rtConHijos]
  ,
  procCola [] ~=? ([] :: [Int])
  ,
  procCola [1] ~=? []
  , 
  procCola [1,2,3] ~=? [2,3]
  , 
  procHijosRose rtConHijos ~=? [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]
  ,
  procHijosRose rtSinHijos ~=? []
  ,
  procHijosAT atNil ~=? []
  ,
  procHijosAT atConHijos ~=? [(Tern 2 Nil Nil Nil), (Tern 3 Nil Nil Nil), (Tern 4 Nil Nil Nil)]
  ,
  procHijosAT atSinHijos ~=? [Nil, Nil, Nil]
  ,
  procRaizTrie trieBool ~=? [Just True]
  ,
  procRaizTrie trieRaizVacia ~=? [Nothing]
  ,
  procRaizTrie trieSinHijos ~=? [Just 4]
  ,
  procSubTries trieBool 
    ~=? [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
  ,
  procSubTries trieSinHijos ~=? []
  ,
  procSubTries trieRaizVacia ~=? [('a', TrieNodo (Just 1) [])]
  ]

-- Auxiliares para tests
sumaNodosAT :: (Num a) => AT a -> a
sumaNodosAT = foldAT 0 (\r rI rM rD -> r + rI + rM + rD)

sumaNodosRT :: (Num a) => RoseTree a -> a
sumaNodosRT = foldRose (\r recHijos -> r + sum recHijos)

sumaNodosTrie :: (Num a) => Trie a -> a
sumaNodosTrie = foldTrie (\r recHijos -> case r of
    Just n -> n + sum (map (\(char, num) -> num) recHijos)
    Nothing -> sum (map (\(char, num) -> num) recHijos))

esNilAT :: AT a -> Bool
esNilAT Nil = True
esNilAT _ = False 

tiene4Hijos :: RoseTree a -> Bool
tiene4Hijos r = length (procHijosRose r) == 4
 
testsEj2 = test [ 
  sumaNodosAT atConHijos ~=? 10
  , 
  sumaNodosAT atSinHijos ~=? 3
  ,
  sumaNodosRT rtConHijos ~=? 15
  ,
  sumaNodosRT rtSinHijos ~=? 3
  ,
  sumaNodosTrie trieSinHijos ~=? 4
  ,
  sumaNodosTrie trieRaizVacia ~=? 1
  ,
  sumaNodosTrie trieEj6ConNum ~=? 10
  ]

testsEj3 = test [ 
  unoxuno [3,1,4,1,5,9]   
    ~=? [[3],[1],[4],[1],[5],[9]]
  ,
  unoxuno [] ~=? ([] :: [[Int]]) 
  ,
  unoxuno [2] ~=? [[2]]
  ,
  sufijos [1,2,3] ~=? [[1,2,3], [2,3], [3], []]
  ,
  sufijos "Plp" ~=? ["Plp", "lp", "p", ""]
  ,
  sufijos [2] ~=? [[2], []]
  ,
  sufijos [] ~=? ([[]] :: [[Int]]) 
  ]

testsEj4 = test [ 
  preorder atEj4       
    ~=? [16,1,9,7,2,14,0,3,6,10,8,5,4]
  ,
  postorder atEj4  
    ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16]  
  ,
  inorder atEj4
    ~=? [9,7,1,2,0,3,14,6,16,8,5,10,4]                 
  ]

testsEj5 = test [ 
  preorderRose rtConHijos ~=? [1,2,3,4,5]
  ,
  preorderRose rtSinHijos ~=? [3]        
  ,                               
  preorderRose rtConUnHijo ~=? [4,1]
  ,
  hojasRose rtConHijos ~=? [2,3,4,5]
  ,
  hojasRose rtSinHijos ~=? [3]
  ,
  hojasRose rtConUnHijo ~=? [1]
  ,
  ramasRose rtConHijos ~=? [[1,2],[1,3],[1,4],[1,5]]
  ,
  ramasRose rtSinHijos ~=? [[3]]
  ,
  ramasRose rtConUnHijo ~=?  [[4,1]]
  ]

testsEj6 = test [ 
  caminos trieEj6 ~=? ["", "a", "b", "ba", "bad", "c"]
  ,
  caminos trieRaizVacia ~=? ["", "a"]
  ,
  caminos trieSinHijos ~=? [""]
  ]

testsEj7 = test [ 
  palabras trieEj6 ~=? ["a", "ba", "c"]
  ,
  palabras trieSinHijos ~=? [""]
  ,
  palabras trieRaizVacia ~=? ["a"]                                         
  ]

testsEj8a = test [ 
  ifProc esNilAT procVacio procId atNil ~=? []
  ,
  ifProc esNilAT procVacio procId atEj4 ~=? [atEj4]                                          
  ,
  ifProc tiene4Hijos procHijosRose procVacio rtConHijos ~=? [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]
  ,
  ifProc tiene4Hijos procHijosRose procVacio rtConUnHijo ~=? []
  ]
testsEj8b = test [ 
  (postorder ++! preorder) atEj4 ~=? postorder atEj4 ++ preorder atEj4
  ,
  (postorder ++! preorder) atConHijos ~=? postorder atConHijos ++ preorder atConHijos
  ,
  (preorderRose ++! hojasRose) rtConHijos ~=? preorderRose rtConHijos ++ hojasRose rtConHijos
  ]
testsEj8c = test [ 
  ((\z->[0..z]) .! (map (+1))) [1,3] ~=? [0,1,2,0,1,2,3,4]                                        
  ]


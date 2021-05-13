-- Clase 6
productoria :: [Int] -> Int
productoria [] = 1
productoria (h:t) = h * productoria t

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (h:t) = h + n : sumarN n t

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (h:t) =  h + h : sumarN h t

ultimo :: [Int] -> Int
ultimo [e] = e
ultimo (h:t) = ultimo t

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo (h:t) = h + ultimo t : sumarN (ultimo t) t

pares :: [Int] -> [Int]
pares [] = []
pares (h:t) | even(h) = h : pares t
            | otherwise = pares t

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n [] = []
quitarTodas n (h:t) | n == h = quitarTodas n t
                    | otherwise = h : quitarTodas n t

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (h:t) | quitarTodas h t == t = hayRepetidos t
                   | otherwise = True

maximo :: [Int] -> Int
maximo [e] = e
maximo (h:t) | h >= head t = maximo (h : quitarTodas (head t) t )
             | otherwise = maximo t

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar l = maximo l : ordenar (quitarTodas  (maximo l) l)

--Clase 7
type Set a = [a]

iguales :: Set Int -> Set Int -> Bool
iguales (h:t) c = pertenece h c || iguales t c

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece n c | n == head c = True
              | otherwise = pertenece n (tail c)

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC _ [] = False
perteneceC n c | n == head c = True
               | otherwise = perteneceC n (tail c)

agregar :: Int -> Set Int -> Set Int
agregar n c | n `pertenece` c = c
            | otherwise = n : c

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC n c | n `perteneceC` c = c
             | otherwise = n : c

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True
incluido s c = head s `pertenece` c && incluido (tail s) c

union :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
union [] y = y
union (h:t) y | h `perteneceC` y = union t y
              | otherwise = union t (h:y)

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos e [] = [[]]
agregarATodos e (h:t) = agregarC (agregar e h) (agregarATodos e t)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (h:t) = union (partes t) (agregarATodos h (partes t))
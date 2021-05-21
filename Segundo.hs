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

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True
incluido s c = head s `pertenece` c && incluido (tail s) c

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos e [] = [[]]
agregarATodos e (h:t) = agregar (agregar e h) (agregarATodos e t)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (h:t) = union (partes t) (agregarATodos h (partes t))

--Clase 8
agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante k [] = []
agregarElementoAdelante k (xs:xss) = agregar (k:xs) (agregarElementoAdelante k xss)

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _ = []
agregarElementosAListas (x:xs) c = union (agregarElementoAdelante x c) (agregarElementosAListas xs c)

variaciones :: Set Int -> Int -> Set [Int]
variaciones _ 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k-1))

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn l n 0 = l
insertarEn l n i | i == 1 = n:l
                 | otherwise = (head l) : (insertarEn (tail l) n (i-1))

insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos l n 1 = agregar (insertarEn l n 1) vacio
insertarEnCadaPos l n i = agregar (insertarEn l n i) (insertarEnCadaPos l n (i-1))

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] c = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = union (insertarEnCadaPos xs c (length xs + 1)) (insertarEnCadaPosDeTodasLasListas xss c)

permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (x:xs) = insertarEnCadaPosDeTodasLasListas (permutaciones xs) x

armarConjuntoDeListas :: Int -> Set [Int]
armarConjuntoDeListas 1 = [[1]]
armarConjuntoDeListas n = agregar [n] (armarConjuntoDeListas (n-1))

agregarCadaBolitaA :: Int -> [Int] -> Set [Int]
agregarCadaBolitaA 1 c = agregar (1:c) vacio
agregarCadaBolitaA b c = agregar (b:c) (agregarCadaBolitaA (b-1) c)

agregarCadaBolitaACadaComb :: Int -> Set [Int] -> Set [Int]
agregarCadaBolitaACadaComb b [] = []
agregarCadaBolitaACadaComb b (xs:xss) = union (agregarCadaBolitaA b xs) (agregarCadaBolitaACadaComb b xss)

agregarCadaBolitaACadaCombPrimera :: Int -> Set [Int] -> Set [Int]
agregarCadaBolitaACadaCombPrimera b [] = []
agregarCadaBolitaACadaCombPrimera b (xs:xss) = union (agregarCadaBolitaA b xs) (agregarCadaBolitaACadaComb b xss)

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas b c | b == 1 = armarConjuntoDeListas c
                   | otherwise = agregarCadaBolitaACadaComb c (bolitasEnCajas (b-1) c)

armarConjuntoDeListasPrimera :: Int -> Set [Int]
armarConjuntoDeListasPrimera 1 = [[1]]
armarConjuntoDeListasPrimera n = agregar [1] (armarConjuntoDeListasPrimera (n-1))

bolitasEnCajasPrimeraLlena :: Int -> Int -> Set [Int]
bolitasEnCajasPrimeraLlena b c | b == 1 = armarConjuntoDeListas c
                               | otherwise = agregarCadaBolitaACadaCombPrimera c (bolitasEnCajasPrimeraLlena (b-1) c)

-- bECPL 2 3 -> {[1,1], [1,2], [1,3]} U {[1,1], [2,1], [3,1]}
-- bECPL 3 3-> {[1,1,1], [1,1,2], [1,1,3], [1,2,1], [1,2,2], [1,2,3], [1,3,1], [1,3,2], [1,3,3], [2,1,1], [2,1,2]}
type Posicion = [Int]
type Jugada = (Int, Int) -- (pos, cant)

-- EJERCICIO 1: Recibe una posición p, una jugada válida j y devuelve la posición obtenida al realizar dicha jugada.
jugar :: Posicion -> Jugada -> Posicion
jugar p (i, n)
    | i == 1 = (head p - n) : tail p
    | otherwise = head p : (jugar (tail p) (i-1, n))

-- EJERCICIO 2: Recibe una posición p y devuelve el conjunto de jugadas válidas a partir de p.
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas n = posiblesJugadasDesde 1 n

posiblesJugadasDesde :: Int -> Posicion -> [Jugada]
posiblesJugadasDesde i [] = []
posiblesJugadasDesde i (x:xs) = (posiblesJugadasEn i x) `unirJugadas` (posiblesJugadasDesde (i+1) xs)

posiblesJugadasEn :: Int -> Int -> [Jugada]
posiblesJugadasEn i 0 = []
posiblesJugadasEn i x = (i, x) : (posiblesJugadasEn i (x-1))

unirJugadas :: [Jugada] -> [Jugada] -> [Jugada]
unirJugadas [] t = t
unirJugadas s t = unirJugadas (tail s) (head s : t)

-- EJERCICIO 3: Decide si una posición p es ganadora.
esPosicionGanadora :: Posicion -> Bool

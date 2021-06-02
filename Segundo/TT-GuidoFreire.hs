type Posicion = [Int]

type Jugada = (Int, Int)

-- EJERCICIO 1: Recibe una posición p, una jugada válida j y devuelve la posición obtenida al realizar dicha jugada.
jugar :: Posicion -> Jugada -> Posicion
jugar p (i, n)
  | i == 1 = (head p - n) : tail p
  | otherwise = head p : jugar (tail p) (i -1, n)

-- EJERCICIO 2: Recibe una posición p y devuelve el conjunto de jugadas válidas a partir de p.
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas n = posiblesJugadasDesde 1 n

posiblesJugadasDesde :: Int -> Posicion -> [Jugada]
posiblesJugadasDesde i [] = []
posiblesJugadasDesde i (x : xs) = posiblesJugadasEn i x `unirJugadas` posiblesJugadasDesde (i + 1) xs

posiblesJugadasEn :: Int -> Int -> [Jugada]
posiblesJugadasEn i 0 = []
posiblesJugadasEn i x = (i, x) : posiblesJugadasEn i (x -1)

unirJugadas :: [Jugada] -> [Jugada] -> [Jugada]
unirJugadas [] t = t
unirJugadas s t = unirJugadas (tail s) (head s : t)

-- EJERCICIO 3: Decide si una posición p es ganadora.
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = hayJugadaGanadora p (posiblesJugadas p)

hayJugadaGanadora :: Posicion -> [Jugada] -> Bool
hayJugadaGanadora _ [] = False
hayJugadaGanadora [] _ = False
hayJugadaGanadora [1] _ = True
hayJugadaGanadora p j = not (esPosicionGanadora (jugar p (head j))) || hayJugadaGanadora p (tail j)

-- EJERCICIO 4: Recibe una posición ganadora p y devuelve una jugada que dejaría al rival en una posición no ganadora.
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p = cualJugadaGanadora p (posiblesJugadas p)

cualJugadaGanadora :: Posicion -> [Jugada] -> Jugada
cualJugadaGanadora p j
  | not (esPosicionGanadora (jugar p (head j))) = head j
  | otherwise = cualJugadaGanadora p (tail j)

-- EJERCICIO 5: Recibe una posición p (no necesariamente ganadora) y devuelve la cantidad de jugadas ganadoras partiendo de p.
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p = cuantasJugadasGanadoras p (posiblesJugadas p)

cuantasJugadasGanadoras :: Posicion -> [Jugada] -> Int
cuantasJugadasGanadoras p [] = 0
cuantasJugadasGanadoras p j
  | not (esPosicionGanadora (jugar p (head j))) = 1 + cuantasJugadasGanadoras p (tail j)
  | otherwise = cuantasJugadasGanadoras p (tail j)
{-

TRABAJO PRACTICO 1
"LA CONJETURA DE GOLDBACH"

- GUIDO FREIRE -
TURNO TARDE

-}

-- EJERCICIO 1: Recibe un numero natural n y devuelve True si y solo si el n es par, mayor que 2 y suma de dos numeros primos o False en caso contrario.
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n
  | n <= 2 || odd n = False
  | otherwise = satisfaceGoldbachDesde n 2

-- Recibe dos numeros naturales y devuelve True si y solo si existen dos primos que sumados sean iguales a n
satisfaceGoldbachDesde :: Integer -> Integer -> Bool
satisfaceGoldbachDesde n k
  | k > div n 2 = False
  | esPrimo (n - k) = True
  | otherwise = satisfaceGoldbachDesde n (minimoPrimoDesde (k + 1))

-- EJERCICIO 2: Recibe un numero natural n par mayor que 2 y devuelve True si y solo si la conjetura es cierta para todos los naturales pares mayores que 2 y menores o iguales que n o False en caso contrario.
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n
  | n == 4 = True
  | satisfaceGoldbach n = verificarConjeturaHasta (n - 2)
  | otherwise = False

-- EJERCICIO 3: Recibe un numero natural n par mayor que 2 y devuelve un par ordenado (a,b) de numeros primos tales que a + b == n
descomposicionEnPrimos :: Integer -> (Integer, Integer)
descomposicionEnPrimos n = descomposicionEnPrimosDesde n 2

-- Reimplementacion de la funcion satisfaceGoldbachPreciso, devolviendo el par ordenado en lugar de un booleano
descomposicionEnPrimosDesde :: Integer -> Integer -> (Integer, Integer)
descomposicionEnPrimosDesde n k
  | esPrimo (n - k) = (k, n - k)
  | otherwise = descomposicionEnPrimosDesde n (minimoPrimoDesde (k + 1))

-- EJERCICIO 4: Recibe un numero natural n par mayor que 2 y devuelve la cantidad de pares ordenados (a, b) de numeros primos tales que a + b == n
numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n = numeroDeDescomposicionesDesde n (fst (descomposicionEnPrimos n))

--Recibe dos numeros naturales y devuelve el numero de descomposiciones de n desde k
numeroDeDescomposicionesDesde :: Integer -> Integer -> Integer
numeroDeDescomposicionesDesde n k
  | k > div n 2 = 0
  | 2 * k == n = 1 + numeroDeDescomposicionesDesde n (fst (descomposicionEnPrimosDesde n (k + 1)))
  | otherwise = 2 + numeroDeDescomposicionesDesde n (fst (descomposicionEnPrimosDesde n (k + 1)))

-----------------------------------------------------------------------------------------------------------------------------------------------------------

-- Las funciones de aca en adelante se utilizan para encontrar el minimo primo desde un numero (Clase 5)
esPrimoDesde :: Integer -> Integer -> Bool
esPrimoDesde n k
  | mod n k == 0 = False
  | k >= div n 2 = True
  | otherwise = esPrimoDesde n (k + 1)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo 2 = True
esPrimo n = esPrimoDesde n 2

minimoPrimoDesde :: Integer -> Integer
minimoPrimoDesde n
  | esPrimo n = n
  | otherwise = minimoPrimoDesde (n + 1)
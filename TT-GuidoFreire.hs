{-

TRABAJO PRACTICO 1
"LA CONJETURA DE GOLDBACH"

- GUIDO FREIRE -
TURNO TARDE

-}

-- EJERCICIO 1: Recibe un numero natural n y devuelve True si y solo si el n es par, mayor que 2 y suma de dos numeros primos o False en caso contrario.
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach = satisfaceGoldbachPreciso (1, 1)

-- Recibe tres numeros naturales i j n y devuelve True si y solo si el n es par, mayor que 2 y suma del i y j-esimos numeros primos.
-- Intenta con el siguiente i-esimo numero primo en caso contrario, hasta que el mismo es igual al j-esimo numero primo.
-- En ese caso intenta con el siguiente j-esimo numero primo y empieza a contar de nuevo con el i.
satisfaceGoldbachPreciso :: (Integer, Integer) -> Integer -> Bool
satisfaceGoldbachPreciso (i, j) n
  | n <= 2 || mod n 2 /= 0 = False
  | nEsimoPrimo i + nEsimoPrimo j == n = True
  | i == j = satisfaceGoldbachPreciso (1, j + 1) n
  | otherwise = satisfaceGoldbachPreciso (i + 1, j) n

-- EJERCICIO 2: Recibe un numero natural n par mayor que 2 y devuelve True si y solo si la conjetura es cierta para todos los naturales pares mayores que 2 y menores o iguales que n o False en caso contrario.
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n
  | n == 2 = True
  | satisfaceGoldbach n = verificarConjeturaHasta (n - 2)
  | otherwise = False

-- EJERCICIO 3: Recibe un numero natural n par mayor que 2 y devuelve un par ordenado (a,b) de numeros primos tales que a + b == n
descomposicionEnPrimos :: Integer -> (Integer, Integer)
descomposicionEnPrimos = descomposicionEnPrimosPreciso (1, 1)

-- Reimplementacion de la funcion satisfaceGoldbachPreciso, devolviendo los i y j-esimos primos en lugar de un booleano
descomposicionEnPrimosPreciso :: (Integer, Integer) -> Integer -> (Integer, Integer)
descomposicionEnPrimosPreciso (i, j) n
  | nEsimoPrimo i + nEsimoPrimo j == n = (nEsimoPrimo i, nEsimoPrimo j)
  | i == j = descomposicionEnPrimosPreciso (1, j + 1) n
  | otherwise = descomposicionEnPrimosPreciso (i + 1, j) n

-- EJERCICIO 4: Recibe un numero natural n par mayor que 2 y devuelve la cantidad de pares ordenados (a, b) de numeros primos tales que a + b == n
numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones = numeroDeDescomposicionesDesde (1, 1)

numeroDeDescomposicionesDesde :: (Integer, Integer) -> Integer -> Integer
numeroDeDescomposicionesDesde (i, j) n
  | nEsimoPrimo j + 2 > n = 0
  | fst (descomposicionEnPrimosPreciso (i, j) n) == snd (descomposicionEnPrimosPreciso (i, j) n) = 1 + numeroDeDescomposicionesDesde (1, snd (descomposicionEnPrimosPrecisoIJ (i, j) n) + 1) n
  | otherwise = 2 + numeroDeDescomposicionesDesde (1, snd (descomposicionEnPrimosPrecisoIJ (i, j) n) + 1) n --REVISAR

-- Reimplementacion de la funcion descomposicionEnPrimosPreciso, devolviendo el i y j en lugar de los i y j-esimos primos
descomposicionEnPrimosPrecisoIJ :: (Integer, Integer) -> Integer -> (Integer, Integer)
descomposicionEnPrimosPrecisoIJ (i, j) n
  | nEsimoPrimo i + nEsimoPrimo j == n = (i, j)
  | i == j = descomposicionEnPrimosPrecisoIJ (1, j + 1) n
  | otherwise = descomposicionEnPrimosPrecisoIJ (i + 1, j) n

-----------------------------------------------------------------------------------------------------------------------------------------------------------

-- Las funciones de aca en adelante se utilizan para encontrar el n-esimo numero primo (Clase 5)
menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k
  | mod n k == 0 = k
  | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n -1))

minimoPrimoDesde :: Integer -> Integer
minimoPrimoDesde n
  | esPrimo n = n
  | otherwise = minimoPrimoDesde (n + 1)
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
dEP :: Integer -> (Integer, Integer)
dEP = dEPD (1, 1)

-- Reimplementacion de la funcion satisfaceGoldbachPreciso, devolviendo los i y j-esimos primos en lugar de un booleano
dEPD :: (Integer, Integer) -> Integer -> (Integer, Integer)
dEPD (i, j) n
  | j + 2 > n = (0, 0)
  | minimoPrimoDesde (i + 1) + minimoPrimoDesde (j + 1) == n = (minimoPrimoDesde (i + 1), minimoPrimoDesde (j + 1))
  | i == j = dEPD (1, j + 1) n
  | otherwise = dEPD (i + 1, j) n

-- EJERCICIO 4: Recibe un numero natural n par mayor que 2 y devuelve la cantidad de pares ordenados (a, b) de numeros primos tales que a + b == n
{-numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones = numeroDeDescomposicionesDesde (2, 2)-}

-- nDD usa numeros primos en vez de posiciones i / j de numeros primos, ahorra calculo de nEsimosPrimos y ademas puede empezar a contar primos desde j=n/2
nDD :: Integer -> Integer
nDD n = nDDD (dEPD(div n 2, div n 2) n) n

nDDD :: (Integer, Integer) -> Integer -> Integer
nDDD (i, j) n
  | j == 0 = 0
  | i == j = 1 + nDDD (dEPD (i, j) n) n
  | otherwise = 2 + nDDD (dEPD (i, j) n) n

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
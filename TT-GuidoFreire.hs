{-

TRABAJO PRACTICO 1
"LA CONJETURA DE GOLDBACH"

- GUIDO FREIRE -
TURNO TARDE

-}

-- Recibe un numero natural n y devuelve True si y solo si el n es par, mayor que 2 y suma de dos numeros primos o False en caso contrario.
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach = satisfaceGoldbachPreciso 1 1

-- Recibe tres numeros naturales i j n y devuelve True si y solo si el n es par, mayor que 2 y suma del i y j-esimos numeros primos.
-- Intenta con el siguiente i-esimo numero primo en caso contrario, hasta que el mismo es igual al j-esimo numero primo.
-- En ese caso intenta con el siguiente j-esimo numero primo y empieza a contar de nuevo con el i.
satisfaceGoldbachPreciso :: Int -> Int -> Integer -> Bool
satisfaceGoldbachPreciso i j n
  | n <= 2 || mod n 2 /= 0 || toInteger(nEsimoPrimo j) - 1 > n = False
  | toInteger(nEsimoPrimo i + nEsimoPrimo j) == n = True
  | i > j = satisfaceGoldbachPreciso 1 (j + 1) n
  | otherwise = satisfaceGoldbachPreciso (i + 1) j n

-- Recibe un numero natural n par mayor que 2 y devuelve True si y solo si la conjetura es cierta para todos los naturales pares mayores que 2 y menores o iguales que n o False en caso contrario.
-- verificarConjeturaHasta :: Integer -> Bool


-- Las funciones de aca en adelante se utilizan para encontrar el n-esimo numero primo (Clase 5)
menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k
  | mod n k == 0 = k
  | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Int -> Bool
esPrimo n = menorDivisor n == n

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n -1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n
  | esPrimo n = n
  | otherwise = minimoPrimoDesde (n + 1)
-- Clase 2
prodInt :: Num a => (a, a) -> (a, a) -> a
prodInt (vx, vy) (wx, wy) = vx * wx + vy * wy

todoMenor :: Ord a => (a, a) -> (a, a) -> Bool
todoMenor v w = fst v < fst w && snd v < snd w

distanciaPuntos :: Floating a => (a, a) -> (a, a) -> a
distanciaPuntos v w = sqrt ((fst v - fst w) ^ 2 + (snd v - snd w) ^ 2)

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

-- Clase 3
fib :: Int -> Int
fib n
  | n > 1 = fib (n -1) + fib (n -2)
  | otherwise = n

parteEntera :: Float -> Int
parteEntera n
  | 0 <= n && n < 1 = 0
  | otherwise = parteEntera (n - 1) + 1

esMultiploTres :: Int -> Bool
esMultiploTres n
  | n - 3 == 0 = True
  | n - 3 < 0 = False
  | otherwise = esMultiploTres (n - 3)

sumaImpares :: Int -> Int
sumaImpares n = n ^ 2 --Como seria recursivamente?

-- Extra 1
digito :: Int -> Integer -> Integer
digito i n = mod (div n (10 ^ (i -1))) 10

sumaDeDigitos :: Integer -> Integer
sumaDeDigitos n
  | div n 10 < 10 = digito 1 n + digito 2 n
  | otherwise = digito 1 n + sumaDeDigitos (div n 10)

-- Clase 4
f1 :: Int -> Integer
f1 0 = 1
f1 n = 2 ^ n + f1 (n -1)

f3 :: Int -> Float -> Float
f3 0 q = 0
f3 n q = f3 (n -1) q + q ^ (2 * n -1) + q ^ (2 * n)

f4 :: Int -> Float -> Float
f4 0 q = 1
f4 n q = f4 (n -1) q + q ^ (2 * n -1) + q ^ (2 * n) + q ^ (n -1)

-- Clase 5
factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n -1)

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n 1 = 1
sumaDivisoresHasta n k
  | mod n k == 0 = sumaDivisoresHasta n (k -1) + k
  | otherwise = sumaDivisoresHasta n (k -1)

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

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

esFactorialDesde :: Integer -> Integer -> Bool
esFactorialDesde 1 k = False -- Por definicion no es factorial
esFactorialDesde n k
  | factorial k == n = True
  | factorial k > n = False
  | otherwise = esFactorialDesde n (k + 1)

esFactorial :: Integer -> Bool
esFactorial n = esFactorialDesde n 1

menorFactDesde :: Integer -> Integer
menorFactDesde n
  | esFactorial n = n
  | otherwise = menorFactDesde (n + 1)
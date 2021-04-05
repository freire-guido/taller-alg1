-- Clase 2
prodInt :: Num a => (a, a) -> (a, a) -> a
prodInt (vx, vy) (wx, wy) = vx * wx + vy * wy

todoMenor :: Ord a => (a, a) -> (a, a) -> Bool
todoMenor v w = fst v < fst w && snd v < snd w

distanciaPuntos :: Floating a => (a, a) -> (a, a) -> a
distanciaPuntos v w = sqrt ((fst v - fst w)^2 + (snd v - snd w)^2)

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

-- Clase 3
fib :: Int -> Int
fib n | n > 1 = fib(n-1) + fib(n-2)
      | otherwise = n

parteEntera :: Float -> Int 
parteEntera n | 0 <= n && n < 1 = 0
              | otherwise = parteEntera (n - 1) + 1

esMultiploTres :: Int -> Bool
esMultiploTres n | n - 3 == 0 = True
                 | n - 3 < 0 = False
                 | otherwise = esMultiploTres (n -3)

sumaImpares :: Int -> Int
sumaImpares n = n^2 --Como seria recursivamente?
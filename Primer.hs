-- Clase 2
prodInt :: Num a => (a, a) -> (a, a) -> a
prodInt (vx, vy) (wx, wy) = vx * wx + vy * wy

todoMenor :: Ord a => (a, a) -> (a, a) -> Bool
todoMenor v w = fst v < fst w && snd v < snd w

distanciaPuntos :: Floating a => (a, a) -> (a, a) -> a
distanciaPuntos v w = sqrt ((fst v - fst w)^2 + (snd v - snd w)^2)

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)
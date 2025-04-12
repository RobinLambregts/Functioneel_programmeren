-- 1.1

opsplits :: Integral a => a -> [a]
opsplits 0 = []
opsplits x = opsplits (x `div` 10) ++ [x `mod` 10]

-- 1.2

uniek :: Eq a => [a] -> [a]
uniek [] = []
uniek (x:xs) = if x `elem` xs then uniek xs else x : uniek xs

-- 1.3

gemeenschappelijk :: Eq a => [a] -> [a] -> [a]
gemeenschappelijk [] [] = []
gemeenschappelijk [] _ = []
gemeenschappelijk _ [] = []
gemeenschappelijk (x:xs) ys = if x `elem` ys && not (x `elem` xs) then x : gemeenschappelijk xs ys else gemeenschappelijk xs ys

-- 1.4 (1)

i :: Eq a => [a] -> [a] -> [a]
i [] [] = []
i [] _ = []
i _ [] = []
i (x:xs) (y:ys) = if x == y then x : i xs ys else i xs ys

-- 1.4 (2)

keren :: (Num a1, Eq a2) => [a2] -> [a2] -> a1
keren [] ys = 0;
keren xs [] = 0;
keren (x:xs) (y:ys)
    | x == y    = keren xs ys + 1
    | otherwise = keren xs ys

-- iets moeilijkere oefeningen

-- 2.1 

fastExp :: (Num a, Integral b) => a -> b -> a
fastExp _ 0 = 1
fastExp x n
  | even n    = fastExp (x * x) (n `div` 2)
  | otherwise = x * fastExp x (n - 1)

-- 2.2

euclides :: Integral a => a -> a -> a
euclides a 0 = a
euclides a b = euclides b (a `mod` b)

-- oefeningen met hulpvariabele

-- 3.1

-- gelukkig getal staat op toledo

-- oefeningen met tuple

-- 4.1

{-
minmaxavg [] = (0, 0, 0)
minmaxavg (x:xs) = mma x x x 1 xs
    where
        mma min max sum n [] = (min, max, sum / n)
        mma min max sum n (x:xs) = mma (min `min` x) (max `max` x) (sum + x) (n + 1) xs
-}

-- 4.2

nulpunten :: (Ord a, Floating a) => a -> a -> a -> (a, a)
nulpunten a b c
    | d < 0     = error "Geen nulpunten"
    | d == 0    = (x, x)
    | otherwise = (x1, x2)
    where
        d = b^2 - 4 * a * c
        x = -b / (2 * a)
        x1 = (-b + sqrt d) / (2 * a)
        x2 = (-b - sqrt d) / (2 * a)

-- 4.3

voorkomers [] = []
voorkomers (x:xs) =
    let andere = voorkomers xs
        voegToe x [] = [(x,1)]
        voegToe x ((y,n):vs) = if x==y then (x,n+1):vs else (y,n) : voegToe x vs
    in voegToe x andere




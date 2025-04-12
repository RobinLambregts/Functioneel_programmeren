--1.1

halveer :: [a] -> [a]
halveer [] = []
halveer (x:y:xs) = x : halveer xs
halveer [x] = [x]

--1.2

verdubbel :: [a] -> [a]
verdubbel [] = []
verdubbel (x:xs) = x : x : verdubbel xs

--1.3 helper

isInList :: Eq a => a -> [a] -> Bool
isInList x [] = False
isInList x (y:ys) = if x == y then True else isInList x ys

--1.3

ontdubbel :: Eq a => [a] -> [a]
ontdubbel [] = []
ontdubbel (x:xs) = if isInList x xs then ontdubbel xs else x : ontdubbel xs

--1.4

producten :: Num a => [a] -> [a] -> a
producten [] [] = 0
producten (x:xs) (y:ys) = x * y + producten xs ys

--1.5

deellijst :: Num a => [[a]] -> [a]
deellijst [] = []
deellijst (x:xs) = sum x : deellijst xs

--2.1

herSom ::(Num a, Fractional a, Eq a) => a -> a
herSom 0 = 0
herSom (x) = (1 / x) + herSom (x-1)

--2.2

mysterie :: (Num a, Eq a) => a -> a
mysterie 0 = 0


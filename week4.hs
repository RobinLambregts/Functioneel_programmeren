tweeMaal [] = []
tweeMaal (x:xs) = x:x:(tweeMaal xs)

tweeMaalSpecifiek [] = []
tweeMaalSpecifiek xs = tweeMaal' xs []

tweeMaal' :: [Int] -> [Int] -> [Int]
tweeMaal' [] xs = reverse xs
tweeMaal' (x:xs) ys = tweeMaal' xs (x:x:ys)

data Temperatuur = Warm | Normaal | Koud
data Seizoen = Lente | Zomer | Herfst | Winter

weer Zomer = Warm
weer Winter = Koud
weer _ = Normaal


data Geslacht = M | V | X deriving (Show, Eq, Ord)
data Mens = Mens String Integer Geslacht deriving (Show, Eq, Ord)

data Punt = Punt {
                x :: Float,
                y :: Float
            }                deriving (Show, Eq, Ord)

pt1 = Punt 3 4

afstand (Punt x1 y1) (Punt x2 y2) = sqrt (dx*dx + dy*dy)
   where dx = x1 - x2
         dy = y1 - y2


type Naam = String
type Leeftijd = Integer

data Mens2 = Mens2 { naam :: Naam,
                     leeftijd :: Leeftijd,
                     sexe :: Geslacht
} deriving (Show, Eq, Ord)

maakJan leeftijd = Mens2 "Jan" leeftijd M

data Point a = Pt a a deriving (Show, Eq, Ord)

afstandPt (Pt x1 y1) (Pt x2 y2) = sqrt (dx * dx + dy * dy)
    where  dx = x2 - x1
           dy = y2 - y1

data SimpleVorm = Cirkel Float | Rechthoek Float Float deriving (Show, Eq, Ord)

isRond (Cirkel _) = True
isRond (Rechthoek _ _) = False

-- Oefening: bereken de oppervlake van een simpele vorm

oppervlakte :: SimpleVorm -> Float
oppervlakte (Cirkel r) = pi * r * r
oppervlakte (Rechthoek l b) = l * b



--- Oefeningen  met examenscores ---

-- 1. Maak een type Examenscore met naam van vak, een punt op 20 en het aantal studiepunten van het vak

data ExamenScore = ExamenScore String Integer Integer deriving (Show, Eq, Ord)

-- 2. Maak een functie die een tuple van 3 waarden (string + 2 getallen) omzet in een Examenscore

omzetten :: (String, Integer, Integer) -> ExamenScore
omzetten (vak, score, studiepunten) = ExamenScore vak score studiepunten

-- 3. Maak een functie die een lijst van tuples omzet in een lijst van Examenscores

omzettenLijst :: [(String, Integer, Integer)] -> [ExamenScore]
omzettenLijst [] = []
omzettenLijst ((vak, score, studiepunten):xs) = omzetten (vak, score, studiepunten) : omzettenLijst xs

-- 4. Pas die functies toe om volgende lijsten om te zetten
student1 = [("MP",14,20),("A",9,4),("B",12,6),("C",18,5),("D",11,4),("E",13,4),("F",12,5),("G",12,4),("H",14,4),("I",15,4)]
student2 = [("MP",14,20),("A",7,4),("B",12,6),("C",18,5),("D",11,4),("E",13,4),("F",12,5),("G",12,4),("H",14,4),("I",15,4)]
student3 = [("MP",14,20),("A",9,4),("B",9,6),("C",8,5),("D",11,4),("E",13,4),("F",12,5),("G",12,4),("H",14,4),("I",15,4)]
student4 = [("MP",14,20),("A",17,4),("B",12,6),("C",18,5),("D",16,4),("E",14,4),("F",19,5),("G",12,4),("H",15,4),("I",17,4)]
student5 = [("MP",11,20),("A",11,4),("B",11,6),("C",11,5),("D",11,4),("E",11,4),("F",11,5),("G",11,4),("H",11,4),("I",11,4)]


-- 5. Schrijf een functie die het gewogen gemiddelde van examenscores + de graad terug geeft
-- Berekening van de graad:
--      minstens één vak met hoogstens 7/20 => niet-geslaagd 
--      meer dan 10% van de studiepunten een 8 of 9 => niet-geslaagd
--      geslaagd en meer dan 85% => grootste onderscheiding
--      geslaagd en tussen 76% en 85% => grote onderscheiding
--      geslaagd en meer dan 68% => onderscheiding
--      geslaagd en minder dan 68% => voldoening

gewogenGemiddelde :: [ExamenScore] -> (Float, String)
gewogenGemiddelde scores = (fromIntegral (sum punten) / fromIntegral (sum studiepunten), graad)
    where punten = [score * studiepunten | ExamenScore _ score studiepunten <- scores]
          studiepunten = [studiepunten | ExamenScore _ _ studiepunten <- scores]
          graad
            | (minimum [score | ExamenScore _ score _ <- scores]) <= 7 = "Niet geslaagd"
            | (sum [1 | ExamenScore _ score _ <- scores, score >= 8 && score <= 9]) > sum studiepunten `div` 10 = "Niet geslaagd"
            | gemiddelde > 0.85 = "Grootste onderscheiding"
            | gemiddelde > 0.76 = "Grote onderscheiding"
            | gemiddelde > 0.68 = "Onderscheiding"
            | otherwise = "Voldoening"
          gemiddelde = fromIntegral (sum punten) / fromIntegral (sum studiepunten)

-- 6. Schrijf een functie die gegeven examenscores de namen van de vakken teruggeeft die hernomen moeten worden

hernemen :: [ExamenScore] -> [String]
hernemen scores = [vak | ExamenScore vak score _ <- scores, score < 10]

-- tussendia over Maybe en Either

-- 7. Pas de functie van 6 aan zodat Nothing terug gegeven wordt als geen vakken hernomen moeten worden (ipv [])

hernemen2 :: [ExamenScore] -> Maybe [String]
hernemen2 scores = if null vakken then Nothing else Just vakken
    where vakken = [vak | ExamenScore vak score _ <- scores, score < 10]

-- 8. Combineer de functie 5 en 6 zodat de functie ofwel graad + gewogen gemiddelde terug geeft wanneer de student geslaagd is, 
--    ofwel de lijst met namen van vakken die hernomen moet worden als de student niet geslaagd is

gewogenGemiddeldeOfHernemen :: [ExamenScore] -> Either (Float, String) [String]
gewogenGemiddeldeOfHernemen scores =
    if graad == "Niet geslaagd"
    then Right (hernemen scores)
    else Left (gemiddelde, graad)
  where
    (gemiddelde, graad) = gewogenGemiddelde scores




data Tree a = Leaf a
            | Branch a (Tree a) (Tree a)
            deriving (Show, Eq, Ord)



boom1 = Branch "Kris" (Branch "Louis" (Leaf "Jos") (Leaf "Maria"))
                      (Leaf "Celine")


boom2 = Branch 10 (Branch 6 (Branch 4 (Leaf 1) (Leaf 5))
                            (Branch 15 (Leaf 1) (Branch 17 (Leaf 16) (Leaf 19))))
                  (Leaf 1)

-- Oefeningen op type Tree a


-- Bereken het aantal knopen van een boom 
aantalKnopen :: Tree a -> Integer
aantalKnopen (Leaf _) = 1
aantalKnopen (Branch _ l r) = 1 + aantalKnopen l + aantalKnopen r

-- Bereken hoe dikwijls een element voorkomt in de boom: --aantalKeer :: b -> Tree b -> Integer
aantalKeer :: Eq a => a -> Tree a -> Integer
aantalKeer x (Leaf y) = if x == y then 1 else 0
aantalKeer x (Branch y l r) = if x == y then 1 + aantalKeer x l + aantalKeer x r else aantalKeer x l + aantalKeer x r

-- Zoek of een element in een boom zit
zoek :: Eq a => a -> Tree a -> Bool
zoek x (Leaf y) = x == y
zoek x (Branch y l r) = x == y || zoek x l || zoek x r

-- Zoek het kleinste element in een boom
kleinste :: Ord a => Tree a -> a
kleinste (Leaf x) = x
kleinste (Branch x l r) = minimum [x, kleinste l, kleinste r]


data Boom a b  = Boom (Either a b)
                | Tak (Either a b) (Boom a b) (Boom a b)
                deriving (Show, Eq, Ord)

-- Oefeningen op type Boom a b
-- Implementeer aantalKeren :: a -> b -> Boom a b -> (Int, Int)

{-
AantalKeren :: (Eq a, Eq b) => a -> b -> Boom a b -> (Int, Int)
AantalKeren x y (Boom (Left z)) = if x == z then 1 else 0
AantalKeren x y (Boom (Right z)) = if y == z then 1 else 0
AantalKeren x y (Tak (Left z) l r) = if x == z then 1 + AantalKeren x y l + AantalKeren x y r else AantalKeren x y l + AantalKeren x y r
AantalKeren x y (Tak (Right z) l r) = if y == z then 1 + AantalKeren x y l + AantalKeren x y r else AantalKeren x y l + AantalKeren x y r
-}



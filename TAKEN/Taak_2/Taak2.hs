-- TYPES
data Voetballer = Voetballer { aanval :: Double, verdediging :: Double, keepen :: Double } deriving (Eq, Show)

data VoetbalTeam = VoetbalTeam { spelers :: [Voetballer], ranking :: Integer } deriving (Eq, Show)

data Match = Match { team1 :: VoetbalTeam, team2 :: VoetbalTeam, score :: (Integer, Integer) } deriving (Eq, Show)

class Comparable a where
  gelijkwaardigheid :: a -> a -> Double
  afstand    :: a -> a -> Double

-- HELPERFUNCTIES
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

bestTeam :: Match -> VoetbalTeam
bestTeam m = if uncurry (>) (score m) then team1 m else team2 m

worstTeam :: Match -> VoetbalTeam
worstTeam m = if uncurry (>) (score m) then team2 m else team1 m

-- INSTANCE FUNCTIES
instance Comparable Voetballer where
  gelijkwaardigheid a b = 1 - maximum 
    [ abs (mean [aanval a - afstand a b, aanval b - afstand a b])
    , abs (mean [verdediging a - afstand a b, verdediging b - afstand a b])
    , abs (mean [keepen a - afstand a b, keepen b - afstand a b])
    ]
  afstand a b = abs (mean [aanval a, verdediging a, keepen a] - mean [aanval b, verdediging b, keepen b])

instance Comparable VoetbalTeam where
  gelijkwaardigheid a b = mean [gelijkwaardigheid x y | x <- spelers a, y <- spelers b]
  afstand a b = abs (fromIntegral (ranking a) - fromIntegral (ranking b)) * gelijkwaardigheid a b

instance Comparable Match where
  gelijkwaardigheid a b = mean
    [ gelijkwaardigheid (bestTeam a) (bestTeam b)
    , gelijkwaardigheid (worstTeam a) (worstTeam b)
    ]

  afstand a b =
    let diffA = abs $ uncurry (-) (score a)
        diffB = abs $ uncurry (-) (score b)
    in abs (fromIntegral (diffA - diffB)) * gelijkwaardigheid a b

-- ANDERE FUNCTIES
filterGelijkwaardig :: Comparable a => [a] -> Double -> a -> [a]
filterGelijkwaardig lijst drempel referentie =
  [x | x <- lijst, gelijkwaardigheid x referentie >= drempel]

respecteertDriehoeksongelijkheid :: Comparable a => [a] -> Bool
respecteertDriehoeksongelijkheid xs =
  and [afstand x z <= afstand x y + afstand y z | x <- xs, y <- xs, z <- xs]

kanBereiken :: (Comparable a, Eq a) => [a] -> a -> a -> Double -> Bool
kanBereiken lijst start doel maxStap =
  zoekPad lijst start doel maxStap [start] []

zoekPad :: (Comparable a, Eq a) => [a] -> a -> a -> Double -> [a] -> [a] -> Bool
zoekPad _ huidig doel _ _ _
  | huidig == doel = True
zoekPad lijst huidig doel maxStap bezocht _
  | huidig `elem` bezocht = False
zoekPad lijst huidig doel maxStap bezocht _
  | null buren = False
  | otherwise = any (\b -> zoekPad lijst b doel maxStap (huidig : bezocht) []) buren
  where
    buren = [x | x <- lijst, x /= huidig, afstand huidig x <= maxStap]


  
  

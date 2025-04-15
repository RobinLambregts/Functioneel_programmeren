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

bestOvereenkomendeSpelers :: Comparable a => [a] -> [a] -> [Double]
bestOvereenkomendeSpelers [] _ = []
bestOvereenkomendeSpelers _ [] = []
bestOvereenkomendeSpelers (x:xs) ys =
  let beste = maximum [gelijkwaardigheid x y | y <- ys]
      gefilterd = [y | y <- ys, gelijkwaardigheid x y /= beste]
  in beste : bestOvereenkomendeSpelers xs gefilterd
  
-- INSTANCE FUNCTIES
instance Comparable Voetballer where
  gelijkwaardigheid a b = 
    let verschillen = [aanvalDiff, verdedigingDiff, keepenDiff]
        aanvalDiff = abs (aanval a - aanval b)
        verdedigingDiff = abs (verdediging a - verdediging b)
        keepenDiff = abs (keepen a - keepen b) in
    let gefilterdOnder = [x | x <- verschillen, x <= afstand a b]
        gefilterdBoven = [x | x <- verschillen, x > afstand a b] 
    in 1 - mean (gefilterdOnder ++ gefilterdBoven ++ gefilterdBoven ++ gefilterdBoven)
  afstand a b = abs (
    mean [aanval a, verdediging a, keepen a] - mean [aanval b, verdediging b, keepen b]
    )

instance Comparable VoetbalTeam where
  gelijkwaardigheid a b =
    mean (bestOvereenkomendeSpelers (spelers a) (spelers b))
  afstand a b =
    let rankVerschil = abs (fromIntegral (ranking a) - fromIntegral (ranking b))
        gel = gelijkwaardigheid a b
    in rankVerschil * (1 - gel)


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

-- Dummy data voor 5 verschillende voetballers
noob1 = Voetballer { aanval = 0.10, verdediging = 0.05, keepen = 0.15 }
gemiddeld1 = Voetballer { aanval = 0.95, verdediging = 0.05, keepen = 0.95 }
verdediger1 = Voetballer { aanval = 0.05, verdediging = 0.80, keepen = 0.05 }
aanvaller1 = Voetballer { aanval = 0.90, verdediging = 0.40, keepen = 0.25 }
keeper1 = Voetballer { aanval = 0.20, verdediging = 0.35, keepen = 0.95 } 

-- Dummy data voor 5 verschillende voetbalteams
voetbalteamZwak = VoetbalTeam { spelers = [noob1, noob1, noob1], ranking = 1 }
voetbalteamGemiddeld = VoetbalTeam { spelers = [gemiddeld1, gemiddeld1, verdediger1], ranking = 5 }
voetbalteamVerdedigend = VoetbalTeam { spelers = [verdediger1, verdediger1, keeper1], ranking = 6 }
voetbalteamAanvallend = VoetbalTeam { spelers = [aanvaller1, aanvaller1, gemiddeld1], ranking = 7 }
voetbalteamTop = VoetbalTeam { spelers = [aanvaller1, verdediger1, keeper1], ranking = 10 }

-- Dummy data voor 5 verschillende wedstrijden
wedstrijd1 = Match { team1 = voetbalteamZwak, team2 = voetbalteamGemiddeld, score = (0, 3) }
wedstrijd2 = Match { team1 = voetbalteamGemiddeld, team2 = voetbalteamVerdedigend, score = (1, 1) }
wedstrijd3 = Match { team1 = voetbalteamVerdedigend, team2 = voetbalteamAanvallend, score = (0, 2) }
wedstrijd4 = Match { team1 = voetbalteamAanvallend, team2 = voetbalteamTop, score = (1, 4) }
wedstrijd5 = Match { team1 = voetbalteamZwak, team2 = voetbalteamTop, score = (0, 6) }



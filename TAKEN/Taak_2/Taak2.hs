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
  in 
       beste : bestOvereenkomendeSpelers xs gefilterd

  
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
    max 
    (mean (bestOvereenkomendeSpelers (spelers a) (spelers b)))
    (mean (bestOvereenkomendeSpelers (spelers b) (spelers a)))
  afstand a b =
    let rankVerschil = abs (fromIntegral (ranking a) - fromIntegral (ranking b))
        gel = gelijkwaardigheid a b
    in rankVerschil * gel

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
  zoekPad lijst start doel maxStap []

zoekPad :: (Comparable a, Eq a) => [a] -> a -> a -> Double -> [a] -> Bool
zoekPad alleElementen huidig doel maxStap bezocht
  | huidig == doel = True
  | huidig `elem` bezocht = False
  | otherwise = any (\buur -> zoekPad alleElementen buur doel maxStap (huidig : bezocht)) geldigeBuren
  where
    geldigeBuren = [x | x <- alleElementen,
                        x /= huidig,
                        gelijkwaardigheid huidig x > 1 - maxStap]



-- Dummy data
noob1 = Voetballer { aanval = 0.10, verdediging = 0.05, keepen = 0.15 }
gemiddeld1 = Voetballer { aanval = 0.95, verdediging = 0.05, keepen = 0.95 }
verdediger1 = Voetballer { aanval = 0.05, verdediging = 0.80, keepen = 0.05 }
aanvaller1 = Voetballer { aanval = 0.90, verdediging = 0.40, keepen = 0.25 }
keeper1 = Voetballer { aanval = 0.20, verdediging = 0.35, keepen = 0.95 } 

teamA = VoetbalTeam { spelers = [noob1, verdediger1, gemiddeld1], ranking = 3 }
teamB = VoetbalTeam { spelers = [noob1, gemiddeld1, verdediger1], ranking = 6 }
teamC = VoetbalTeam { spelers = [gemiddeld1, keeper1], ranking = 7 }
teamD = VoetbalTeam { spelers = [noob1, aanvaller1, verdediger1], ranking = 4 }
teamE = VoetbalTeam { spelers = [gemiddeld1, aanvaller1], ranking = 8 }

wedstrijdA = Match { team1 = teamA, team2 = teamB, score = (1, 2) }
wedstrijdB = Match { team1 = teamC, team2 = teamD, score = (3, 1) }
wedstrijdC = Match { team1 = teamE, team2 = teamA, score = (0, 0) }
wedstrijdD = Match { team1 = teamB, team2 = teamC, score = (2, 2) }
wedstrijdE = Match { team1 = teamD, team2 = teamE, score = (1, 4) }




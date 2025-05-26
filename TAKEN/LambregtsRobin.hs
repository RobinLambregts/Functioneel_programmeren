{-
BRONVERMELDING

Syntax m.b.v. cursus, ChatGPT en Copilot
Herhalingen laten genereren door AI
Algoritmes en logica laten nakijken door ChatGPT 
    -> deze daarna aangepast met eigen kennis,
       geinspireerd door de uitleg van ChatGPT
       aangezien deze niet altijd correct was.
    -> ook de uitleg van Copilot was niet altijd correct
       maar gaf wel een goede basis.
-}

-------------------------------------------------IMPORTS-------------------------------------------------
import System.IO
import System.Random
import System.Random.Shuffle (shuffleM)
import Data.List (nub, subsequences, sortBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

-------------------------------------------------TAAK 1-------------------------------------------------

data Maanlander = Maanlander
    { hoogte :: Integer
    , brandstof :: Integer
    , fmax :: Integer
    , vmax :: Integer
    , g :: Integer
    , strategie :: String
    } deriving (Show, Eq, Ord)

-- Shared helper
teSnel :: Integer -> Integer -> Integer -> Integer -> Bool
teSnel snelheid fmax hoogte g
    | hoogte <= 0     = snelheid > 0
    | snelheid <= 0   = False
    | otherwise       =
        let nieuweSnelheid = snelheid - (fmax - g)
            nieuweHoogte = hoogte - nieuweSnelheid
        in teSnel nieuweSnelheid fmax nieuweHoogte g

-- helper strategie 2
berekenMinimaleTegengas :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
berekenMinimaleTegengas snelheid fmax hoogte g tegengas vmax =
    let nieuweSnelheid = min (snelheid - tegengas + g) vmax
        nieuweHoogte = hoogte - nieuweSnelheid
    in if teSnel nieuweSnelheid fmax nieuweHoogte g then
        berekenMinimaleTegengas snelheid fmax hoogte g (tegengas + 1) vmax
    else
        tegengas

-- STRATEGIEEN
strategie1 :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Maanlander
strategie1 fmax vmax beginSnelheid beginHoogte gVal brandstof =
    if teSnel beginSnelheid fmax (beginHoogte - beginSnelheid) gVal then
        let nieuweSnelheid = beginSnelheid - fmax + gVal
            nieuweHoogte = beginHoogte - nieuweSnelheid
            nieuweBrandstof = brandstof - fmax
        in Maanlander nieuweHoogte nieuweBrandstof fmax vmax gVal "strategie1"
    else
        let voorlopigeSnelheid = beginSnelheid + gVal
            (nieuweSnelheid, nieuweBrandstof) =
                if voorlopigeSnelheid > vmax then
                    let snelheidNaRemmen = vmax
                        verbruikteBrandstof = voorlopigeSnelheid - snelheidNaRemmen
                    in (snelheidNaRemmen, brandstof - verbruikteBrandstof)
                else
                    (voorlopigeSnelheid, brandstof)
            nieuweHoogte = beginHoogte - nieuweSnelheid
        in Maanlander nieuweHoogte nieuweBrandstof fmax vmax gVal "strategie1"

strategie2 :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Maanlander
strategie2 fmax vmax beginSnelheid beginHoogte gVal brandstof =
    if teSnel beginSnelheid fmax (beginHoogte - beginSnelheid) gVal then
        let tegengas = berekenMinimaleTegengas beginSnelheid fmax beginHoogte gVal 0 vmax
            snelheidNaRemmen = beginSnelheid - tegengas + gVal
            nieuweBrandstof = max 0 (brandstof - tegengas)
            nieuweHoogte = beginHoogte - snelheidNaRemmen
        in Maanlander nieuweHoogte nieuweBrandstof fmax vmax gVal "strategie2"
    else
        let voorlopigeSnelheid = beginSnelheid + gVal
            (nieuweSnelheid, nieuweBrandstof) =
                if voorlopigeSnelheid > vmax then
                    let snelheidNaRemmen = vmax
                        verbruikteBrandstof = voorlopigeSnelheid - snelheidNaRemmen
                    in (snelheidNaRemmen, brandstof - verbruikteBrandstof)
                else
                    (voorlopigeSnelheid, brandstof)

            nieuweHoogte = beginHoogte - nieuweSnelheid
        in Maanlander nieuweHoogte nieuweBrandstof fmax vmax gVal "strategie2"

strategie3 :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Maanlander
strategie3 fmax vmax beginSnelheid beginHoogte gVal brandstof = do
    let vLimiet = fmax - gVal
    if vLimiet < beginSnelheid then
        let nieuweSnelheid = beginSnelheid - vLimiet + gVal
            nieuweHoogte = beginHoogte - nieuweSnelheid
            nieuweBrandstof = brandstof - beginSnelheid - nieuweSnelheid
        in Maanlander nieuweHoogte nieuweBrandstof fmax vmax gVal "strategie3"
    else 
        let nieuweSnelheid = beginSnelheid + gVal
            nieuweHoogte = beginHoogte - nieuweSnelheid
        in Maanlander nieuweHoogte brandstof fmax vmax gVal "strategie3"

-- main function
land :: [Maanlander] -> IO Integer
land [] = return 0
land [_] = return 0
land (x:y:xs) = do
    let beginSnelheid = hoogte y - hoogte x
        beginHoogte = hoogte x
        gVal = g x
    if beginHoogte < 0 then do
        return 0
    else if beginHoogte == 0 then do
        return (brandstof x - beginSnelheid)
    else if brandstof x <= 0 then do
        return 0
    else if beginHoogte < beginSnelheid + gVal 
        && beginHoogte > beginSnelheid - fmax x + gVal
        && beginSnelheid - fmax x + gVal <= 0 then do
        return (brandstof x - 2 * beginSnelheid + beginHoogte)
    else
        let strategieFunctie = case strategie x of
                "strategie1" -> strategie1
                "strategie2" -> strategie2
                "strategie3" -> strategie3
            nieuweLander = strategieFunctie (fmax x) (vmax x) beginSnelheid beginHoogte gVal (brandstof x)
        in land (nieuweLander : x : y : xs)

-- Main1 simulation
simulatie :: IO ()
simulatie = do
    putStrLn "Geef Beginhoogte:"
    beginHoogte <- readLn
    putStrLn "Geef Beginbrandstof:"
    beginBrandstof <- readLn
    putStrLn "Geef maximale snelheid:"
    vmax <- readLn
    putStrLn "Geef maximale kracht:"
    fmax <- readLn
    putStrLn "Geef zwaartekracht:"
    g <- readLn

    let maanlander = Maanlander beginHoogte beginBrandstof fmax vmax g "strategie1"
        maanlander2 = Maanlander (beginHoogte - g) beginBrandstof fmax vmax g "strategie1"
        maanlanderLijst = [maanlander2, maanlander]

    brandstof1 <- land maanlanderLijst
    let maanlanderLijst = [maanlander2 { strategie = "strategie2" }, maanlander { strategie = "strategie2" }]
    brandstof2 <- land maanlanderLijst
    let maanlanderLijst = [maanlander2 { strategie = "strategie3" }, maanlander { strategie = "strategie3" }]
    brandstof3 <- land maanlanderLijst

    putStrLn "\n=== Resultaten ==="
    putStrLn $ "Strategie 1 (max rem):     " ++ show (beginBrandstof - brandstof1) ++ " brandstof verbruikt"
    putStrLn $ "Strategie 2 (min rem):     " ++ show (beginBrandstof - brandstof2) ++ " brandstof verbruikt"
    putStrLn $ "Strategie 3 (simpel limiet): " ++ show (beginBrandstof - brandstof3) ++ " brandstof verbruikt"

main1 :: IO ()
main1 = simulatie

----------------------------------------------TAAK 2--------------------------------------------------

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

-- Dummy data -> gegenereert door ChatGPT
larsA = Voetballer { aanval = 0.10, verdediging = 0.05, keepen = 0.15 }
stijnVC = Voetballer { aanval = 0.95, verdediging = 0.05, keepen = 0.95 }
robinL = Voetballer { aanval = 0.05, verdediging = 0.80, keepen = 0.05 }
krisA = Voetballer { aanval = 0.90, verdediging = 0.40, keepen = 0.25 }
karelK = Voetballer { aanval = 0.20, verdediging = 0.35, keepen = 0.95 } 

teamA = VoetbalTeam { spelers = [larsA, robinL, stijnVC], ranking = 3 }
teamB = VoetbalTeam { spelers = [larsA, stijnVC, robinL], ranking = 6 }
teamC = VoetbalTeam { spelers = [stijnVC, karelK], ranking = 7 }
teamD = VoetbalTeam { spelers = [larsA, krisA, robinL], ranking = 4 }
teamE = VoetbalTeam { spelers = [stijnVC, krisA], ranking = 8 }

wedstrijdA = Match { team1 = teamA, team2 = teamB, score = (1, 2) }
wedstrijdB = Match { team1 = teamC, team2 = teamD, score = (3, 1) }
wedstrijdC = Match { team1 = teamE, team2 = teamA, score = (0, 0) }
wedstrijdD = Match { team1 = teamB, team2 = teamC, score = (2, 2) }
wedstrijdE = Match { team1 = teamD, team2 = teamE, score = (1, 4) }   

---------------------------------------------------TAAK 3--------------------------------------------------

-- Datatypes
data Woning = Woning { eigenaar :: String, compatibel :: [String] } deriving (Eq, Show)
data Verhuizing = Verhuizing { van :: String, naar :: String } deriving (Eq, Show)

-- Input genereren
genereerInputbestand :: FilePath -> IO ()
genereerInputbestand filepath = do
  putStrLn "Minimaal aantal compatibele woningen:"
  ondergrens <- readLn
  putStrLn "Maximaal aantal compatibele woningen:"
  bovengrens <- readLn
  putStrLn "Aantal inschrijvingen (0 <= x <= 26):"
  aantal <- readLn
  let eigenaars = take aantal ['a'..'z']
  regels <- mapM (genereerLijn ondergrens bovengrens eigenaars) eigenaars
  writeFile filepath (unlines regels)

genereerLijn :: Int -> Int -> [Char] -> Char -> IO String
genereerLijn ondergrens bovengrens eigenaars eigenaar = do
  let mogelijke = filter (/= eigenaar) eigenaars
  n <- randomRIO (ondergrens, min bovengrens (length mogelijke))
  gekozen <- take n <$> shuffleM mogelijke
  let gekozenMetEigenaar = eigenaar : gekozen
  let compatStr = concat $ voegKommasTussen $ map (:[]) gekozenMetEigenaar
  return $ eigenaar : ": " ++ compatStr

voegKommasTussen :: [String] -> [String]
voegKommasTussen [] = []
voegKommasTussen [x] = [x]
voegKommasTussen (x:xs) = (x ++ ", ") : voegKommasTussen xs

-- Input lezen
leesWoningen :: FilePath -> IO [Woning]
leesWoningen bestand = do
  inhoud <- readFile bestand
  return $ map parseLijn (lines inhoud)

parseLijn :: String -> Woning
parseLijn lijn =
  let (e:rest) = splitOn ":" lijn
      compat = map (filter (/= ' ')) $ splitOn "," (concat rest)
  in Woning e (filter (/= e) compat)

-- output schrijven
schrijfOutput :: FilePath -> [Verhuizing] -> IO ()
schrijfOutput bestand verhuizingen = do
  let inhoud = unlines $ map (\(Verhuizing v n) -> v ++ " -> " ++ n) verhuizingen
  writeFile bestand inhoud

-- max verhuizingen bepalen
alleVerhuizingen :: [Woning] -> [Verhuizing]
alleVerhuizingen woningen = [Verhuizing (eigenaar w) c | w <- woningen, c <- compatibel w]

alleNaarWoningenKomenUitVan :: [String] -> [String] -> Bool
alleNaarWoningenKomenUitVan vanLijst = all (`elem` vanLijst)


isGeldigeSet :: [Verhuizing] -> Bool
isGeldigeSet verhuizingen =
  let vanLijst = map van verhuizingen
      naarLijst = map naar verhuizingen
      printable = verhuizingen
  in trace (show printable) $
     (length vanLijst == length (nub vanLijst)) &&
     (length naarLijst == length (nub naarLijst)) &&
     alleNaarWoningenKomenUitVan vanLijst naarLijst

maximaleVerhuizingen :: [Verhuizing] -> [Verhuizing]
maximaleVerhuizingen verhuizingen =
  let alleCombinaties = subsequences verhuizingen
      geldigeSets = filter isGeldigeSet alleCombinaties
  in maximumByLength geldigeSets

maximumByLength :: [[a]] -> [a]
maximumByLength = foldl (\acc x -> if length x > length acc then x else acc) []

-- Output printen
printVerhuizingen :: [Verhuizing] -> IO ()
printVerhuizingen verhuizingen = do
  putStrLn $ "Max aantal verhuizingen: " ++ show (length verhuizingen)
  mapM_ (\(Verhuizing v n) -> putStrLn $ "- " ++ v ++ " -> " ++ n) verhuizingen

-- Main functie
main3 :: IO ()
main3 = do
  genereerInputbestand "Input_Taak_3.txt"
  woningen <- leesWoningen "Input_Taak_3.txt"
  let kandidaten = alleVerhuizingen woningen
      maxSet = maximaleVerhuizingen kandidaten
  schrijfOutput "Output_Taak_3.txt" maxSet
  putStrLn $ "Maximaal aantal verhuizingen: " ++ show (length maxSet)
  print "Details in output bestand"


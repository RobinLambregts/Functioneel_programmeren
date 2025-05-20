import System.IO
import System.Random
import System.Random.Shuffle (shuffleM)
import Data.List (nub, subsequences, sortBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

-- Datatypes
data Woning = Woning { eigenaar :: String, compatibel :: [String] } deriving (Eq, Show)
data Verhuizing = Verhuizing { van :: String, naar :: String } deriving (Eq, Show)

-- Input genereren
genereerInputbestand :: IO ()
genereerInputbestand = do
  putStrLn "Minimaal aantal compatibele woningen:"
  ondergrens <- readLn
  putStrLn "Maximaal aantal compatibele woningen:"
  bovengrens <- readLn
  putStrLn "Aantal inschrijvingen (0 <= x <= 26):"
  aantal <- readLn
  let eigenaars = take aantal ['a'..'z']
  regels <- mapM (genereerLijn ondergrens bovengrens eigenaars) eigenaars
  writeFile "generatedInput.txt" (unlines regels)

genereerLijn :: Int -> Int -> [Char] -> Char -> IO String
genereerLijn ondergrens bovengrens eigenaars eigenaar = do
  let mogelijke = filter (/= eigenaar) eigenaars
  n <- randomRIO (ondergrens, min bovengrens (length mogelijke))
  gekozen <- take n <$> shuffleM mogelijke
  let compatStr = concat $ voegKommasTussen $ map (:[]) gekozen
  return $ eigenaar : ": " ++ eigenaar : ", " ++ compatStr

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

-- max verhuizingen bepalen
alleVerhuizingen :: [Woning] -> [Verhuizing]
alleVerhuizingen woningen = [Verhuizing (eigenaar w) c | w <- woningen, c <- compatibel w]

isGeldigeSet :: [Verhuizing] -> Bool
isGeldigeSet verhuizingen =
  let vanLijst = map van verhuizingen
      naarLijst = map naar verhuizingen
  in (length vanLijst == length (nub vanLijst)) &&
     (length naarLijst == length (nub naarLijst))

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
main :: IO ()
main = do
  genereerInputbestand
  woningen <- leesWoningen "generatedInput.txt"
  let kandidaten = alleVerhuizingen woningen
      maxSet = maximaleVerhuizingen kandidaten
  printVerhuizingen maxSet

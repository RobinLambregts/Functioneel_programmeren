import System.IO
import Data.List.Split (splitOn)
import System.Random

data Woning = Woning { eigenaar :: String, compatibel :: [String], verhuisPlan :: Maybe (Woning, Woning)} deriving (Eq, Show)

{-
INPUTFILE STRUCTUUR
eigenaar1: eigenaarCompat1, eigenaarCompat2, eigenaarCompat3, eigenaarCompat4, eigenaarCompat5
eigenaar2: eigenaarCompat1, eigenaarCompat2
eigenaar3: eigenaarCompat1, eigenaarCompat2, eigenaarCompat3
...
eigenaarN: eigenaarCompat1, ...
-}

inputToWoningen :: String -> [Woning]
inputToWoningen input =
  let linesInput = lines input
      woningLijnen = map (splitOn ":") linesInput
      woningData = map (\[eigenaar, compat] ->
                          (eigenaar, map (filter (/= ' ')) (splitOn "," compat))
                       ) woningLijnen

      woningen = [ Woning eigenaar compat Nothing
                 | (eigenaar, compat) <- woningData ]
  in woningen

genereerInputbestand :: IO ()
genereerInputbestand = do
  let eigenaars = ['a'..'j']
  regels <- sequence [genereerLijn e eigenaars | e <- eigenaars]
  writeFile "generatedInput.txt" (unlines regels)

genereerLijn :: Char -> [Char] -> IO String
genereerLijn eigenaar eigenaars = do
  n <- randomRIO (1, 5) :: IO Int
  compat <- sequence [randomRIO ('a', 'j') | _ <- [1..n]]
  let compatStr = concat (voegKommasTussen (map (: []) compat))
  return (eigenaar : ": " ++ compatStr)

voegKommasTussen :: [String] -> [String]
voegKommasTussen [] = []
voegKommasTussen [x] = [x]
voegKommasTussen (x:xs) = (x ++ ", ") : voegKommasTussen xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let woningen = inputToWoningen input
  genereerInputbestand


-- schaar-steen-papier
import Data.List (sort, group)
data Move = Paper | Rock | Scissors deriving (Eq, Show, Ord)
p = Paper
r = Rock
s = Scissors

type Round = (Move, Move)

-- stap 1: score :: Round -> (Int, Int)
score (speler1, speler2) = wieWint speler1 speler2 

wieWint Paper Rock = (1,0)
wieWint Rock Paper = (0,1)
wieWint Rock Scissors = (1,0)
wieWint Scissors Rock = (0,1)
wieWint Paper Scissors = (0,1)
wieWint Scissors Paper = (1,0)
wieWint _ _ = (0,0)


-- stap 2: bereken de score na een reeks rondes
-- scoreNa :: [Round] -> (Int, Int)
scoreNa rs = foldr combineer (0,0) (map score rs)
  where combineer (sp1, sp2) (sc1, sc2) = (sp1 + sc1, sp2 + sc2)


rondes = [(p,p), (r,s), (r,p), (s,p)] 
rondes2 = [(p,p), (r,s), (r,p), (s,p), (s,r), (s,r)] 

-- stap 3: bereken de persoon die het eerste 'n' rondes heeft gewonnen (geeft 1 of 2 terug)
-- bestOf :: Int -> [Round] -> Int 
bestOf n rs = bestOf2 n rs (0,0)
   where bestOf2 n [] _ = error "niet genoeg rondes"
         bestOf2 n (ronde:rs) (sp1, sp2) 
            | n == sp1 + sc1  = 1
            | n == sp2 + sc2  = 2
            | otherwise = bestOf2 n rs (sp1+sc1, sp2+sc2)
            where  (sc1, sc2) = score ronde             

winnaar1 = bestOf 2 rondes
winnaar2 = bestOf 3 rondes2

-- stap 4: 'AI': versla met een slimme strategie je tegenstander 
type Strategy = [Move] -> Move -- lijst in omgekeerde volgorde

-- voorbeelden: altijdSteen, altijd eerste parameter, hetzelfde als de tegenstander vorige keer deed, wint van laatste beurt
altijdSteen _ = Rock

hetZelfde [] = Paper  -- De eerste zet is in principe random, bijvoorbeeld Paper.
hetZelfde (zet:zetten) = zet

wintVan [] = Scissors -- De eerste zet is in principe random, bijvoorbeeld Scissors.
wintVan (Paper:zetten) = Scissors
wintVan (Scissors:zetten) = Rock
wintVan (Rock:zetten) = Paper

-- OEFENING 1
mostFrequent :: (Eq a, Ord a) => [a] -> a
mostFrequent [] = error "leeg"
mostFrequent xs = go (group (sort xs)) (head xs, 0)
    where
        go [] (val, _) = val
        go (g:gs) (val, maxCount)
            | length g > maxCount = go gs (head g, length g)
            | otherwise = go gs (val, maxCount)

wintMeestal [] = Paper -- terug iets willekeurig
wintMeestal moves = -- kies de zet die wint van de zet die al het meeste gezet is
      let meest = mostFrequent moves
      in case meest of
            Paper -> Scissors
            Rock -> Paper
            Scissors -> Rock

-- NIEUW
-- genereer een oneindige lijst van rondes
-- generateRounds :: (Strategy, Strategy) -> [Round]
generateRounds (speler1, speler2) = nieuweZet [] []
   where nieuweZet moves1 moves2 = let zet1 = speler1 moves2
                                       zet2 = speler2 moves1 
                                   in (zet1, zet2):(nieuweZet ((speler1 moves2):moves1) ((speler2 moves1):moves2))

generateNRounds n strategieen = take n (generateRounds strategieen)



type Strategy2 = [Move] -> [Move] -- lijst in omgekeerde

naDrieKeerSchaarSteen _ = [Scissors, Scissors, Scissors] ++ (repeat Rock) 
zelfde xs = Rock:xs
roteer xs = [Rock, Paper, Scissors] ++ (roteer xs)
steenPapier xs = Rock:(papierSteen xs)
papierSteen xs = Paper:(steenPapier xs)
wintVanVorige zetten = Scissors:(map winnaar zetten) -- De eerste zet is in principe random, bijvoorbeeld Scissors.
  where winnaar Rock = Paper
        winnaar Paper = Scissors
        winnaar Scissors = Rock        

generateRoundsStrat2 :: (Strategy2, Strategy2) -> [Round]
generateRoundsStrat2 (p1, p2) =  zip moves1 moves2
    where moves1 = p1 moves2
          moves2 = p2 moves1 
          
generateNRounds2 n strategieen = take n (generateRoundsStrat2 strategieen)
          
-- OEFENING 2
-- wintMeestal2 :: [Move] -> [Move]
          
          
-- MAAR 
-- wintVanVorige begint met een zet, en pakt dan de winnaar van de vorige zet
-- Dat kan misschien slimmer, want waarom vergelijk je niet direct met de huidige zet?
wintVanVals zetten = map winnaar zetten
  where winnaar Rock = Paper
        winnaar Paper = Scissors
        winnaar Scissors = Rock        

-- uitleg: vals spelen gaat omdat lazy evaluation kijkt naar welke expressie eerst nodig is. 
-- wintVanVals heeft de waarde nodig van de zet van de tegenstander en dwingt dus de tegenstander om eerst de zet te doen.
-- Als je automatischGame (wintVanVals, wintVanVals) probeert, zie je dat beide functies op elkaar wachten. 
-- Twee valsspelers lukt dus niet.

-- Je hebt dus een aanpak nodig die dwingt dat je de eerste zet berekent van speler 1 én speler2 voordat je kijkt wie er wint.
-- Dit kan met de seq-operator.

eerlijkeRondes (speler1, speler2) = zip moves1 moves2 
    where moves1 = fairMoves speler1 moves2
          moves2 = fairMoves speler2 moves1 

fairMoves speler otherMoves = myMoves 
    where myMoves = speler (firstMine otherMoves myMoves)
          firstMine (otherMove:otherMoves) (myMove:myMoves) = (myMove `seq` otherMove):(firstMine otherMoves myMoves)    

          
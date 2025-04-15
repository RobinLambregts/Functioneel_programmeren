import Data.Maybe

class Overlappend a where
   isOverlappend :: a -> a -> Bool
   unie :: a -> a -> a

class Overlappend a => Splitsbaar a where
   doorsnede :: a -> a -> Maybe a -- nothing als de doorsnede leeg is

   
{-- rechthoek --}   
data Rechthoek = Rechthoek { links :: Int,
                             rechts :: Int,
                             onder :: Int,
                             boven :: Int}
    deriving (Eq, Ord, Show)
    -- oorsprong assenstelstel linksboven

instance Overlappend Rechthoek where
    isOverlappend rh1 rh2 = (overlaptH1 || overlaptH2)
                        &&
                       (overlaptV1 || overlaptV2)
        where l1 = links rh1
              l2 = links rh2
              r1 = rechts rh1
              r2 = rechts rh2
              o1 = onder rh1
              o2 = onder rh2
              b1 = boven rh1
              b2 = boven rh2
              overlaptH1 = l2 <= r1 && r2 >= l1
              overlaptH2 = l1 <= r2 && r1 >= l2
              overlaptV1 = o2 <= b1 && b2 >= o1
              overlaptV2 = o1 <= b2 && b1 >= o2
    unie rh1 rh2 = Rechthoek { links = min (links rh1) (links rh2),
                               rechts = max (rechts rh1) (rechts rh2),
                               boven = max (boven rh1) (boven rh2),
                               onder = min (onder rh1) (onder rh2) }

instance Splitsbaar Rechthoek where
    doorsnede rh1 rh2 = if isOverlappend rh1 rh2 then Just kleineRechthoek
                                                 else Nothing
        where kleineRechthoek = Rechthoek { links = max (links rh1) (links rh2),
                                            rechts = min (rechts rh1) (rechts rh2), 
                                            boven = min (boven rh1) (boven rh2),
                                            onder = max (onder rh1) (onder rh2) }
                                            
rh1 = Rechthoek 4 10 2 5
rh2 = Rechthoek 2 8 (-2) 3 
rh3 = Rechthoek 5 8 (-2) 3
rh4 = Rechthoek 5 14 (-2) 3
rh5 = Rechthoek 2 14 (-2) 3
rh6 = Rechthoek 1 3 3 4
rh7 = Rechthoek 14 15 1 8
   
rhs = [rh1, rh2, rh3, rh4, rh5, rh6, rh7]

{- 
implementatie van Overlappend en Splitsbaar voor Int  
    - twee getallen overlappen wanneer ze gemeenschappelijke delers hebben groter dan 1
    - unie = product van alle priemdelers, bv. unie 8 (2*2*2) en 64 (2*2*2*2) = 64 (2*2*2*2), 
                                               unie 24 (2*2*2*3) en 20 (2*2*3*5) = 120 (2*2*2*3*5)
                 grootste gemene deler -> standaard functie in Haskell  
    - doorsnede = produ ct van de gemeenschappelijke priemdelers,
                        bv. doorsnede (8::Int) (2*2*2) en (64::Int) (2*2*2*2) = 8 (2*2*2), 
                            doorsnede (24::Int) (2*2*2*3) en (60::Int) (2*2*3*5) = 12 (2*2*3)        
-}
delers n = [x | x <- [1..n], mod n x == 0]   

priemDelers n = (primers 2 n)
  where primers 1 1 = [1]
        primers kandidaatDeler m 
           | kandidaatDeler > m        = [] 
           | kandidaatDeler == m       = [kandidaatDeler]
           | mod m kandidaatDeler == 0 = kandidaatDeler:(primers kandidaatDeler (div m kandidaatDeler))
           | otherwise                 = primers (kandidaatDeler+1) n

gemeenschappelijk [] _ = []
gemeenschappelijk _ [] = []
gemeenschappelijk (x:xs) (y:ys) | x == y    = x:(gemeenschappelijk xs ys)
                                | x < y     = gemeenschappelijk xs (y:ys)
                                | otherwise = gemeenschappelijk (x:xs) ys    
                                
union [] xs = xs
union xs [] = xs
union (x:xs) (y:ys) | x == y    = x:(union xs ys)
                    | x < y     = x:(union xs (y:ys))
                    | otherwise = y:(union (x:xs) ys)    

-- (kleine) OEFENING: implementeer de twee type classes voor Int

instance Overlappend Integer where
   -- twee getallen overlappen wanneer ze gemeenschappelijke delers hebben groter dan 1
   isOverlappend a b = length (gemeenschappelijk (priemDelers a) (priemDelers b)) > 0 
   unie a b = product (union (priemDelers a) (priemDelers b))
     
instance Splitsbaar Integer where
   doorsnede a b = if isOverlappend a b  then Just (product (gemeenschappelijk (priemDelers a)
                                                                               (priemDelers b)))
                                         else Nothing

-- EXTRA OEFENING VOOR DE SNELLE STUDENT EN/OF THUIS -- 

--data Leeftijdsgroep = Groep Int Int
data Leeftijdsgroep a = Groep a a

-- instance Ord a => Overlappend (Leeftijdsgroep a) where

    
{------------------------------------
 -  IMPLEMENTATIE VAN DE ALGORITMES -
 ------------------------------------}

-- niet nodig om te declareren, maar wordt hier als tip meegegeven
--recursieveUnie :: Overlappend a => [a] -> [a]

--globaleDoorsnede :: Splitsbaar a => [a] -> Maybe a
globaleDoorsnede [] = Nothing
globaleDoorsnede (x:xs) = snede xs x
   where snede [] result = Just result
         snede (x:xs) result = if isOverlappend x result then snede xs (fromJust (doorsnede x result))
                                                         else Nothing

globaleSnede [] = Nothing
globaleSnede [x] = Just x
globaleSnede (x:y:xs) = if snede == Nothing then Nothing
                                            else globaleSnede ((fromJust snede):xs) 
    where snede = doorsnede x y

durchschnitt [] = Nothing
durchschnitt [x] = Just x
durchschnitt (x:xs) = let einde = durchschnitt xs 
                      in if einde == Nothing then Nothing
                                             else doorsnede x (fromJust einde)
                                             
                                             
recursieveUnie xs = if recursiestap == xs then recursiestap
                                          else recurse recursiestap
   where recurse [] = []
         recurse (x:xs) = 
            let overlaptMet =  [y | y <- xs, isOverlappend x y]
                overlapper = foldr unie x overlaptMet 
                overlaptNiet = [y | y <- xs, not (isOverlappend x y)]
            in overlapper:(recurse overlaptNiet)
         recursiestap = recurse xs



{-*******************************************************************
  *******************************************************************
  *******************************************************************
  
  DEEL 2: Kortste afstand tussen twee strings

  *******************************************************************
  *******************************************************************
  *******************************************************************-}

data Edit = Kopieer Char
          | Insert Char
          | Vervang Char Char
          | Verwijder Char
          | VerwijderRest String
          deriving (Eq, Show)
          
transformeer :: String -> String -> [  [Edit]   ]
transformeer "" "" = [ [] ]
transformeer [] ys = [ map Insert ys ]
transformeer xs [] = [ [VerwijderRest xs] ]
transformeer (x:xs) (y:ys) 
   = if x == y then map ((Kopieer x):) (transformeer xs ys)
               else map (\t -> (Insert y):t) (transformeer (x:xs) ys)
                    ++
                    map (\t -> (Vervang x y):t) (transformeer xs ys)
                    ++
                    map (\t -> (Verwijder x):t) (transformeer xs (y:ys))
                    
--goedkoopsteTransformaties :: String -> String -> (Int, [[Edit]])
goedkoopsteTransformaties xs ys =
  let transformaties = transformeer xs ys
      nietKopieer (Kopieer _) = False
      nietKopieer _ = True
      prijs ts = length (filter nietKopieer ts)
      prijzen = map prijs transformaties
      goedkoopste = minimum prijzen
  in (goedkoopste, filter (\ts -> prijs ts == goedkoopste) transformaties)
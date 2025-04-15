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



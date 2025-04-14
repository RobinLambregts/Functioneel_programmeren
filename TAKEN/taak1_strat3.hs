import Control.Concurrent (threadDelay)

data Maanlander = Maanlander
    { hoogte :: Integer
    , brandstof :: Integer
    , fmax :: Integer
    , vmax :: Integer
    , g :: Integer
    } deriving (Show, Eq, Ord)

teSnel :: Integer -> Integer -> Integer -> Integer -> Bool
teSnel snelheid fmax hoogte g
    | hoogte <= 0     = snelheid > 0
    | snelheid <= 0   = False
    | otherwise       =
        let nieuweSnelheid = snelheid - (fmax - g)
            nieuweHoogte = hoogte - nieuweSnelheid
        in teSnel nieuweSnelheid fmax nieuweHoogte g

berekenMinimaleTegengas :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
berekenMinimaleTegengas snelheid fmax hoogte g tegengas vmax = do
    let nieuweSnelheid = min (snelheid - tegengas + g) vmax
        nieuweHoogte = hoogte - nieuweSnelheid
    if teSnel nieuweSnelheid fmax nieuweHoogte g then do
        berekenMinimaleTegengas snelheid fmax hoogte g (tegengas + 1) vmax
    else
        tegengas

land :: [Maanlander] -> IO Integer
land [] = return 0
land [_] = return 0
land (x:y:xs) = do
    threadDelay 1000000
    print x
    let beginSnelheid = hoogte y - hoogte x
        beginHoogte = hoogte x
        gVal = g x
    if beginHoogte < 0 then do
        putStrLn "GECRASHT!"
        return 0
    else if beginHoogte == 0 then do
        putStrLn "Lander is geland"
        return (brandstof x - beginSnelheid)
    else if brandstof x <= 0 then do
        putStrLn "Lander heeft geen brandstof meer"
        return 0
    else if beginHoogte < beginSnelheid + gVal 
        && beginHoogte > beginSnelheid - fmax x + gVal
        && beginSnelheid - fmax x + gVal <= 0 then do
        putStrLn "afremmen en landen"
        return (brandstof x - 2 * beginSnelheid + beginHoogte)
    else if teSnel beginSnelheid (fmax x) (beginHoogte - beginSnelheid) gVal then do
        putStrLn "Lander gaat crashen, minimale tegengas geven"
        let tegengas = berekenMinimaleTegengas beginSnelheid (fmax x) beginHoogte gVal 0 (vmax x)
            snelheidNaRemmen = beginSnelheid - tegengas + gVal
            nieuweBrandstof = max 0 (brandstof x - tegengas)
            nieuweHoogte = beginHoogte - snelheidNaRemmen
            nieuweLander = Maanlander nieuweHoogte nieuweBrandstof (fmax x) (vmax x) gVal
        land (nieuweLander : x : y : xs)
    else do
        putStrLn "Lander gaat niet crashen, geen tegengas geven"
        let voorlopigeSnelheid = beginSnelheid + gVal
            (nieuweSnelheid, nieuweBrandstof, melding) =
                if voorlopigeSnelheid > vmax x then
                    let snelheidNaRemmen = vmax x
                        verbruikteBrandstof = voorlopigeSnelheid - snelheidNaRemmen
                    in (snelheidNaRemmen, brandstof x - verbruikteBrandstof, "Landersnelheid overschrijdt limiet, tegengas toegepast")
                else
                    (voorlopigeSnelheid, brandstof x, "Lander blijft binnen limiet, geen tegengas nodig")
        putStrLn melding

        let nieuweHoogte = beginHoogte - nieuweSnelheid
            nieuweLander = Maanlander nieuweHoogte nieuweBrandstof (fmax x) (vmax x) gVal
        land (nieuweLander : x : y : xs)


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
    let maanlander = Maanlander beginHoogte beginBrandstof fmax vmax g
        maanlander2 = Maanlander (beginHoogte - g) beginBrandstof fmax vmax g
        maanlanderLijst = [maanlander2, maanlander]
    putStrLn "========================="
    putStrLn "SIMULATIE STARTEN"
    putStrLn "========================="
    brandstofOver <- land maanlanderLijst
    putStrLn ("Resterende brandstof: " ++ show brandstofOver)
    putStrLn ("Gebruikte brandstof: " ++ show (beginBrandstof - brandstofOver))
    putStrLn "========================="
    putStrLn "SIMULATIE EINDIGEN"
    putStrLn "========================="

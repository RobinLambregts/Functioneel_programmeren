import Control.Concurrent (threadDelay)

data Maanlander = Maanlander
    { hoogte :: Integer
    , brandstof :: Integer
    , fmax :: Integer
    , vmax :: Integer
    , g :: Integer
    } deriving (Show, Eq, Ord)


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
    else do
        let vLimiet = fmax x - gVal
        if vLimiet < beginSnelheid then do
            putStrLn "afremmen"
            let nieuweSnelheid = beginSnelheid - vLimiet + gVal
                nieuweHoogte = beginHoogte - nieuweSnelheid
                nieuweBrandstof = brandstof x - beginSnelheid - nieuweSnelheid
                nieuweLander = Maanlander nieuweHoogte nieuweBrandstof (fmax x) (vmax x) gVal
            land (nieuweLander : x : y : xs)
        else do
            putStrLn "vrije val"
            let nieuweSnelheid = beginSnelheid + gVal
                nieuweHoogte = beginHoogte - nieuweSnelheid
                nieuweLander = Maanlander nieuweHoogte (brandstof x) (fmax x) (vmax x) gVal
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

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Graph


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

land :: [Maanlander] -> [Maanlander]
land [] = []
land [_] = []
land (x:y:xs)
    | beginHoogte < 0 = []
    | beginHoogte == 0 = (x {brandstof = brandstof x - beginSnelheid}) : y : xs
    | brandstof x <= 0 = []
    | beginHoogte < beginSnelheid + gVal &&
      beginHoogte > beginSnelheid - fmax x + gVal &&
      beginSnelheid - fmax x + gVal <= 0 =
        (x {brandstof = brandstof x - 2 * beginSnelheid + beginHoogte}) : y : xs
    | otherwise =
        let strategieFunctie = case strategie x of
                "strategie1" -> strategie1
                "strategie2" -> strategie2
                "strategie3" -> strategie3
            nieuweLander = strategieFunctie (fmax x) (vmax x) beginSnelheid beginHoogte gVal (brandstof x)
        in land (nieuweLander : x : y : xs)
  where
    beginSnelheid = hoogte y - hoogte x
    beginHoogte = hoogte x
    gVal = g x

main :: IO ()
main = startGUI defaultConfig { jsStatic = Just "static"} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Maanlander simulaties"

    -- Inputvelden
    hoogteInput <- UI.input # set (UI.attr "placeholder") "Beginhoogte"
    brandstofInput <- UI.input # set (UI.attr "placeholder") "Brandstof"
    vmaxInput <- UI.input # set (UI.attr "placeholder") "vmax"
    fmaxInput <- UI.input # set (UI.attr "placeholder") "fmax"
    gInput <- UI.input # set (UI.attr "placeholder") "zwaartekracht"
    hoogteLijst <- UI.span # set (UI.attr "placeholder") "Hoogte"
    brandstofLijst <- UI.span # set (UI.attr "placeholder") "Brandstof"

    -- Knop
    startKnop <- UI.button #+ [string "Start simulatie"]
    show1 <- UI.button #+ [string "details strategie 1"]
    show2 <- UI.button #+ [string "details strategie 2"]
    show3 <- UI.button #+ [string "details strategie 3"]

    -- Resultaatvelden
    resultaat1 <- UI.span
    grafiekContainer1 <- UI.div # set UI.id_ "grafiek1"
    resultaat2 <- UI.span
    grafiekContainer2 <- UI.div # set UI.id_ "grafiek2"
    resultaat3 <- UI.span
    grafiekContainer3 <- UI.div # set UI.id_ "grafiek3"

    -- Layout
    getBody window #+
        [ column [ element hoogteInput, element brandstofInput, element vmaxInput, element fmaxInput, element gInput, element startKnop ]
        , UI.hr
        , grid
            [ [string "Strategie 1: ", element resultaat1]
            , [string "Strategie 2: ", element resultaat2]
            , [string "Strategie 3: ", element resultaat3]
            ]
        , UI.hr
        , grid 
            [[element show1, element show2, element show3]]
        , UI.hr
        , grid
            [ [string "Hoogte:  ", element hoogteLijst]
            , [string "Brandstof:   ", element brandstofLijst]
            ]
        ]

    -- Event handler
    on UI.click startKnop $ \_ -> do
        hoogteStr <- get value hoogteInput
        brandstofStr <- get value brandstofInput
        vmaxStr <- get value vmaxInput
        fmaxStr <- get value fmaxInput
        gStr <- get value gInput

        let readInt s = fromMaybe 0 (readMaybe s :: Maybe Integer)
            hoogteInit = readInt hoogteStr
            brandstofInit = readInt brandstofStr
            vmax = readInt vmaxStr
            fmax = readInt fmaxStr
            g = readInt gStr

            maanlander = Maanlander hoogteInit brandstofInit fmax vmax g "strategie1"
            maanlander2 = maanlander { hoogte = hoogteInit - g }

            simuleer strat = land [maanlander2 { strategie = strat }, maanlander { strategie = strat }]

        let b1 = simuleer "strategie1"
            b2 = simuleer "strategie2"
            b3 = simuleer "strategie3"

        case b1 of
            (eerste:_) -> element resultaat1 # set text (show (brandstofInit - brandstof eerste))
            []         -> element resultaat1 # set text "Simulatie faalde"

        case b2 of
            (eerste:_) -> element resultaat2 # set text (show (brandstofInit - brandstof eerste))
            []         -> element resultaat2 # set text "Simulatie faalde"

        case b3 of
            (eerste:_) -> element resultaat3 # set text (show (brandstofInit - brandstof eerste))
            []         -> element resultaat3 # set text "Simulatie faalde"

        on UI.click show1 $ \_ -> do
            element hoogteLijst # set text (show (reverse (map hoogte b1)))
            element brandstofLijst # set text (show (reverse (map brandstof b1)))

        on UI.click show2 $ \_ -> do
            element hoogteLijst # set text (show (reverse (map hoogte b2)))
            element brandstofLijst # set text (show (reverse (map brandstof b2)))

        on UI.click show3 $ \_ -> do
            element hoogteLijst # set text (show (reverse (map hoogte b3)))
            element brandstofLijst # set text (show (reverse (map brandstof b3)))

-- Hulpfunctie om strings naar integers om te zetten
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing
import Data.Char

-- 1.1

kwadratenSom :: (Floating a, Enum a, Ord a) => a -> a
kwadratenSom n = sum [x*x | x <- [1..(sqrt n)], (x*x) < n]

-- 1.2

--helpfunctie
delers :: Integral a => a -> [a]
delers n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Integral a => a -> [a]
perfect n = [x | x <- [1..n], sum (delers x) == 2*x]

-- 1.3

enkel :: Integral a => [a] -> [a] -> [a]
enkel xs ys = [x | x <- xs, not (elem x ys)] ++ [y | y <- ys, not (elem y xs)]

-- 2.1

ontbinden :: Integral a => a -> [(a, a)]
ontbinden n = [(x, n `div` x) | x <- [1..n], n `mod` x == 0, x <= n `div` x]

-- 2.2

pythagoras :: Integral a => a -> [(a, a, a)]
pythagoras n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2, a <= b, b <= c]

-- 2.3

multi :: Integral a => a -> a -> [a]
multi n x = [x | y <- [1..n]]

-- 2.4

driehoek :: Integral a => a -> [(a, a, a)]
driehoek n = [(x,y,z) | (x,y,z) <- pythagoras (n `div` 2), x+y+z == n]

-- 2.5

ggd a b = maximum [x | x <- [1..a], (a `mod` x == 0) && (b `mod` x == 0)] == 1

ggdIsEen n = [(a,b) | a <- [1..n], b <- [1..n], ggd a b]

-- 3.1

upperCase s = map toUpper s

-- 3.2

upOnly s = filter isUpper s

-- 3.3

isVowel c = elem c "aeiou"
convert l = if isVowel l then toUpper l else l
upperVowels s = map convert s

-- 3.4

sumOfSquares :: Int -> Int
sumOfSquares 0 = 0
sumOfSquares n = (n `mod` 10) ^ 2 + sumOfSquares (n `div` 10)

isHappy :: Int -> Bool
isHappy n = isHappy' n []
  where
    isHappy' :: Int -> [Int] -> Bool
    isHappy' 1 _ = True
    isHappy' x seen
      | x `elem` seen = False
      | otherwise = isHappy' (sumOfSquares x) (x : seen)

happyNumbers n = filter isHappy [1..n]

-- 3.5

isNotPriem :: Int -> Bool
isNotPriem n = [x | x <- [1..n], n `mod` x == 0] == [1, n]

--filterPriem :: [Int] -> [Int]
--filterPriem = filter isPriem

-- 4.1

prod list = foldr (*) 1 list

-- 4.2

delen list = foldl (/) (last list) list


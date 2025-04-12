-- 1.1

import Data.List (group, sort)
import Data.Ord (comparing)

mostFrequent :: (Eq a, Ord a) => [a] -> a
mostFrequent xs = go (group (sort xs)) (head xs, 0)
    where
        go [] (val, _) = val
        go (g:gs) (val, maxCount)
            | length g > maxCount = go gs (head g, length g)
            | otherwise = go gs (val, maxCount)

-- 1.2

herSom :: (Integral a) => a -> a
herSom n = go n 0
    where
        go 0 sum = sum
        go n sum = go (1 `div` n) (sum + 1 `mod` n)
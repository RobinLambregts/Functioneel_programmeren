import Data.Char (ord)

data Complex = Complex { real :: Double, imaginary :: Double } deriving (Eq, Show)

instance Num Complex where
    (Complex a b) + (Complex c d) = Complex (a + c) (b + d)
    (Complex a b) * (Complex c d) = Complex (a * c - b * d) (a * d + b * c)
    abs (Complex a b) = Complex (sqrt (a * a + b * b)) 0
    signum (Complex a b) = let magnitude = sqrt (a * a + b * b) in Complex (a / magnitude) (b / magnitude)
    fromInteger n = Complex (fromInteger n) 0
    negate (Complex a b) = Complex (negate a) (negate b)

------------------------------------------------------------------------------------------------------------------------

class Listable a where
    toList :: a -> [a]

instance Listable Int where
    toList n = [x | x <- [1..n], n `mod` x == 0]

------------------------------------------------------------------------------------------------------------------------


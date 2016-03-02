-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0    = []
  | otherwise = m : toDigitsRev d
    where
      (d, m) = x `divMod` 10

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = []
  | otherwise = toDigits d ++ [m]
    where
      (d, m) = x `divMod` 10

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x
  | length x `mod` 2 == 0 = doubleEvery x True
  | otherwise             = doubleEvery x False

doubleEvery :: [Integer] -> Bool -> [Integer]
doubleEvery [] _           = []
doubleEvery [x] _          = [x]
doubleEvery (x:y:zs) True  = 2*x : y   : doubleEvery zs True
doubleEvery (x:y:zs) False = x   : 2*y : doubleEvery zs False

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- Exercise 4

validate :: Integer -> Bool
validate x = 0 == mod (sumDigits $ doubleEveryOther $ toDigits x) 10

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b  ++ [(a, b)] ++ hanoi (n-1) c b a

-- Exercise 6
-- Frame–Stewart algorithm: https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame.E2.80.93Stewart_algorithm

hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 0 _ _ _ _ = []
hanoi2 n a b c d = hanoi2 k a c b d ++ hanoi (n-k) a b d ++ hanoi2 k c b a d
       where
        k = n - round (sqrt $ fromInteger $ 2*n + 1) + 1

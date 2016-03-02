{-# LANGUAGE FlexibleInstances #-}
-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/06-laziness.pdf

module Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = xs where xs = Cons x xs

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a0 a') (Cons b0 b') = Cons a0 (Cons b0 (interleaveStreams a' b'))

ruler :: Stream Integer
ruler = interleaveStreams zero (interleaveStreams one (interleaveStreams two threeNats))
  where
    zero = streamRepeat 0
    one = streamRepeat 1
    two = streamRepeat 2
    threeNats = streamFromSeed (+1) 3

-- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger a = Cons a (streamRepeat 0)
  negate = streamMap negate
  Cons a0 a' + Cons b0 b'     = Cons (a0 + b0) (a' + b')
  Cons a0 a' * b@(Cons b0 b') = Cons (a0 * b0) (streamMap (*a0) b' + a' * b)

instance Fractional (Stream Integer) where
  a@(Cons a0 a') / b@(Cons b0 b') = Cons (a0 `div` b0) (streamMap (`div` b0) (a' + negate (b' * a / b)))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer
            deriving (Eq)

instance Show Matrix where
  show (Matrix a1 a2 a3 a4) =
    "[" ++ show a1 ++ " " ++ show a2 ++ "]" ++ "\n" ++
    "[" ++ show a3 ++ " " ++ show a4 ++ "]"

instance Num Matrix where
  fromInteger a = Matrix a 0 0 0
  negate (Matrix a1 a2 a3 a4) = Matrix (-a1) (-a2) (-a3) (-a4)
  Matrix a11 a12 a13 a14 + Matrix a21 a22 a23 a24 = Matrix (a11 + a21) (a12 + a22) (a13 + a23) (a14 + a24)
  Matrix a11 a12 a13 a14 * Matrix a21 a22 a23 a24 = Matrix (a11 * a21 + a12 * a23) (a11 * a22 + a12 * a24) (a13 * a21 + a14 * a23) (a13 * a22 + a14 * a24)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getFib $ Matrix 1 1 1 0 ^ n
  where
    getFib (Matrix _ a2 _ _) = a2

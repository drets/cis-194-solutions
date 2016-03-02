-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf

import Data.List

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2)*fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> if even x then (x - 2)*y else y) 1

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate collatz
        where
          collatz x = if even x then x `div` 2 else 3*x + 1

-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr makeTree Leaf

makeTree :: a -> Tree a -> Tree a
makeTree a Leaf = Node 0 Leaf a Leaf
makeTree a (Node _ l m r)
  | height l <= height r = let l' = makeTree a l in Node (1 + height l') l' m r
  | otherwise            = let r' = makeTree a r in Node (1 + height r') l  m r'
  where
    height :: Tree a -> Integer
    height Leaf           = -1
    height (Node h _ _ _) = h

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (\x s -> if x then not s else s) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- http://stackoverflow.com/a/6172270/2517622
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\b g x -> g (f x b)) id xs base

-- Exercise 4
-- https://en.wikipedia.org/wiki/Sieve_of_Sundaram

sieveSundaram :: Integer -> [Integer]
sieveSundaram num = [ 2*x + 1 | x <- genSieve num ]
                  where
                    genCrossed m = [ x | i <- [1..m], j <- [i..m], let x = i + j + 2*i*j, x <= num ]
                    genSieve n = [1..n] \\ genCrossed n

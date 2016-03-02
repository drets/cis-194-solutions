-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
-- File name: Golf.hs

module Golf where

import Data.List

-- Exercise 1

skips :: [a] -> [[a]]
skips xs = [ [ x | (x,i) <- r, i `mod` j == 0 ] | let r = zip xs [1..], (_,j) <- r ]

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima r = [ y | (x,y,z) <- zip3 r (drop 1 r) (drop 2 r), x < y, y > z]

-- Exercise 3

histogram :: [Integer] -> String
histogram xs = unlines [ [ if n >= i then '*' else ' ' | n <- xs' ] | i <- [m,m-1..1] ] ++ "==========\n0123456789\n"
               where
                 xs' = [ length [ y | y <- xs, y == x ] | x <- [0..9] ]
                 m   = maximum xs'

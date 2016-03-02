-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/12-monads.pdf
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import           Control.Monad
import           Control.Monad.Random
import           Data.List

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle root@(Battlefield x' y') = do
  let Battlefield x y = getArmy root
  m <- replicateM x die
  k <- replicateM y die
  let (xs', ys') = unzip $ zipWith (\x y -> if x > y then (0, -1) else (-1, 0)) (sort m) (sort k)
  return (Battlefield (x' + sum xs') (y' + sum ys'))

getArmy :: Battlefield -> Battlefield
getArmy (Battlefield x y) = Battlefield x' y'
  where
    x' = min 3 (max 0 (x - 1))
    y' = min 2 y

-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade root = do
  x <- battle root
  if attackers x < 2 || defenders x == 0
     then return x
     else invade x

-- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb root = do
  let tries = 1000
  xs <- replicateM tries (invade root)
  let wins = length $ filter (== True) (fmap (\x -> defenders x == 0) xs)
  return $ fromIntegral wins / fromInteger (toInteger tries)

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf
-- Employee.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/08-IO/Employee.hs

module Party where

import Data.List
import Data.Tree
import Employee

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs f) = GL (xs ++ [x]) (f + empFun x)

instance Monoid GuestList where
  mempty  = GL [] 0
  GL xs x `mappend` GL ys y = GL (xs ++ ys) (x + y)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x xs) = f x (map (treeFold f) xs)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (bestWith, bestWithout)
  where bestWith = glCons b $ mconcat $ map snd gls
        bestWithout = mconcat $ map (uncurry moreFun) gls

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun  = uncurry moreFun . treeFold nextLevel

-- Exercise 5

calc :: GuestList -> ([String], Integer)
calc (GL xs f) = (sort $ map empName xs, f)

main :: IO ()
main = do
  company <- readFile "company.txt"
  let (names, fun) = calc $ maxFun (read company)
  putStrLn $ "Total fun: " ++ show fun
  mapM_ putStrLn names

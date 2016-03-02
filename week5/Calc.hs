{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf
-- ExprT.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/05-type-classes/ExprT.hs
-- Parser.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/05-type-classes/Parser.hs		
-- StackVM.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/05-type-classes/StackVM.hs

module Calc where

import Data.Maybe
import Parser
import qualified Data.Map.Strict as M
import qualified ExprT as E
import qualified StackVM as VM

-- Exercise 1

eval :: E.ExprT -> Integer
eval (E.Lit n)   = n
eval (E.Add a b) = eval a + eval b
eval (E.Mul a b) = eval a * eval b

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n | n > 0     = True
        | otherwise = False
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

-- Exercise 5

instance Expr VM.Program where
  lit a   = [VM.PushI a]
  add a b = a ++ b ++ [VM.Add]
  mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a _   = Just a
  add a b d = liftMaybe2 (+) (a d) (b d)
  mul a b d = liftMaybe2 (*) (a d) (b d)

liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe2 f (Just a) (Just b) = Just (f a b)
liftMaybe2 _ _ _               = Nothing

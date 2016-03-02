{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/07-folds-monoids.pdf
-- Editor.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/Editor.hs
-- Buffer.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/Buffer.hs
-- Sized.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/Sized.hs
-- StringBuffer.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/StringBuffer.hs
-- StringBufEditor.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/StringBufEditor.hs

module JoinList where

import Buffer
import Data.Monoid
import Editor
import Scrabble
import Sized

-- Exercise 1

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Append m _ _) = m
tag (Single m _)   = m
tag Empty          = mempty

-- Exercise 2

getSize' :: (Sized b, Monoid b) => JoinList b a -> Int
getSize' = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n x
  | n < 0          = Nothing
  | n > getSize' x = Nothing
indexJ _ Empty          = Nothing
indexJ _ (Single _ x)   = Just x
indexJ n (Append _ l r)
  | ls > n       = indexJ n l
  | otherwise    = indexJ (n - ls) r
    where
      ls = getSize' l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x
  | n < 0          = x
  | n > getSize' x = Empty
dropJ _ Empty          = Empty
dropJ _ x@(Single _ _) = x
dropJ n (Append _ l r)
  | ls > n    = dropJ n l +++ r
  | otherwise = dropJ (n - ls) r
    where
      ls = getSize' l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n x
  | n <= 0         = Empty
  | n > getSize' x = x
takeJ _ Empty          = Empty
takeJ _ x@(Single _ _) = x
takeJ n (Append _ l r)
  | ls >= n    = takeJ n l
  | otherwise = l +++ takeJ (n - ls) r
    where
      ls = getSize' l

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where

  toString Empty          = ""
  toString (Single _ x)   = x
  toString (Append _ l r) = toString l ++ "\n" ++ toString r

  fromString x = foldr ((+++) . create) Empty (lines x)
                   where
                     create i = Single (scoreString i, Size 1) i

  line = indexJ

  replaceLine n s x =
    case indexJ n x of
      Nothing -> x
      _       -> takeJ n x +++ Single (scoreString s, Size 1) s +++ dropJ (n + 1) x

  numLines Empty          = 0
  numLines (Single _ _)   = 1
  numLines (Append _ l r) = numLines l + numLines r

  value = getScore . fst . tag

main =
  runEditor editor (Empty :: JoinList (Score, Size) String)

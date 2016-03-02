{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/07-folds-monoids.pdf
-- Editor.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/Editor.hs
-- Buffer.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/Buffer.hs
-- Sized.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/Sized.hs
-- StringBuffer.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/StringBuffer.hs
-- StringBufEditor.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/07-folds-monoids/StringBufEditor.hs

module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score x = case toLower x of
  'a' -> 1
  'b' -> 3
  'c' -> 3
  'd' -> 2
  'e' -> 1
  'f' -> 4
  'g' -> 2
  'h' -> 4
  'i' -> 1
  'j' -> 8
  'k' -> 5
  'l' -> 1
  'm' -> 3
  'n' -> 1
  'o' -> 1
  'p' -> 3
  'q' -> 10
  'r' -> 1
  's' -> 1
  't' -> 1
  'u' -> 1
  'v' -> 4
  'w' -> 4
  'x' -> 8
  'y' -> 4
  'z' -> 10
  _   -> 0

instance Monoid Score where
  mempty  = 0
  mappend = (+)

scoreString :: String -> Score
scoreString = mconcat . map score

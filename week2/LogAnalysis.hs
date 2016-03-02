{-# OPTIONS_GHC -Wall #-}
-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf
-- Log: http://www.seas.upenn.edu/~cis194/spring13/extras/02-ADTs/Log.hs
-- File name: LogAnalysis.hs

module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage m = case words m of
  ("E":x:y:zs) -> LogMessage (Error $ read x) (read y) (unwords zs)
  ("I":x:ys)   -> LogMessage Info             (read x) (unwords ys)
  ("W":x:ys)   -> LogMessage Warning          (read x) (unwords ys)
  _            -> Unknown m

parse :: String -> [LogMessage]
parse =  map parseMessage . lines

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf        = Node Leaf m Leaf
insert msg root
  | t1 > t2   = Node l msg2 (insert msg r)
  | otherwise = Node (insert msg l) msg2 r
                where
                  LogMessage _ t1 _ = msg
                  Node l msg2@(LogMessage _ t2 _) r = root
-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
-- https://en.wikipedia.org/wiki/Tree_traversal#In-order

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf               = []
inOrder (Node l m r)       = inOrder l ++ [m] ++ inOrder r

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = [ msg | LogMessage (Error s) _ msg <- inOrder (build xs), s >= 50 ]

-- Exercise 6
-- λ> testWhatWentWrong parse whatWentWrong "error.log"
-- ["Mustardwatch opened, please close for proper functioning!","All backup mustardwatches are busy","Depletion of mustard stores detected!","Hard drive failure: insufficient mustard","All backup mustardwatches are busy","Twenty seconds remaining until out-of-mustard condition","Ten seconds remaining until out-of-mustard condition","Empty mustard reservoir! Attempting to recover...","Recovery failed! Initiating shutdown sequence"]
-- mustard - hacker name

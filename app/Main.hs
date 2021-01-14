module Main where

import State

main :: IO ()
main = do
  let state = append3ReversedWithCount [1 .. 3] [4 .. 7] [8 .. 10]
  print $ runState state 0

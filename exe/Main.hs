module Main where

import Network

main :: IO ()
main = do
  let numRoots = 3
      numNeighbours = max (numRoots - 1) 8
  simNetwork numRoots numNeighbours

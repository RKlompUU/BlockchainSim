module Main where

import Network

main :: IO ()
main = do
  let numRoots = 8
      numNeighbours = 3
  simNetwork numRoots numNeighbours

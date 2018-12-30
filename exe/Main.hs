module Main where

import Network

main :: IO ()
main = do
  let numRoots = 8
      numNeighbours = 4
  simNetwork numRoots numNeighbours

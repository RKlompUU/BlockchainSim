module Network where


import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State

import Types
import Node
import Admin
import Util

simNetwork :: Int -> Int -> IO ()
simNetwork numRoots numNeighbours = do
  admin <- newChan
  let n = Network {
            ntwrk_admin = admin,
            ntwrk_static = [],
            ntwrk_numNeighbours = numNeighbours,
            ntwrk_nodeC = 0
          }
  flip evalStateT n $ do
    runObserver
    statics <- for [0..numRoots] $ \_ -> spawnNode
    n <- get
    put $ n {ntwrk_static = statics}
    for [1..10] $ \i -> do
      liftIO $ threadDelay (1 * 1000 * 1000)
      liftIO $ putStrLn $ "Tick " ++ show i ++ ".."
    return ()

module Network where


import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad.State

import Types
import Node
import Admin
import Util

simNetwork :: Int -> Int -> IO ()
simNetwork numRoots numNeighbours = do
  admin <- atomically newTChan
  let n = Network {
            ntwrk_admin = admin,
            ntwrk_static = [],
            ntwrk_numNeighbours = numNeighbours,
            ntwrk_nodeC = 0
          }
  flip evalStateT n $ do
    runObserver
    statics <- for [1..numRoots] $ \_ -> spawnNode
    n <- get
    put $ n {ntwrk_static = statics}
    bootstrapStatics
    for [1..10] $ \i -> do
      liftIO $ threadDelay (1000 * 1000)
      liftIO $ putStrLn $ "Tick " ++ show i ++ ".."
      if i == 5
        then liftIO $ atomically $ writeTChan admin EvPrintNetwork
        else return ()
    return ()

bootstrapStatics :: NtwrkState ()
bootstrapStatics = do
  statics <- ntwrk_static <$> get
  liftIO $ bootstrapNodes statics
  where bootstrapNodes [] = return ()
        bootstrapNodes (x:xs) = do
          for xs $ \x' -> do
            msgChan <- atomically $ newTChan
            let req = ConnReq (n_id x) msgChan (n_newConnChan x)
            atomically $ writeTChan (n_newConnChan x') req
          bootstrapNodes xs

module Node where


import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.State
import System.Posix.Unistd

import Types
import Util

-- Initializes, and starts (i.e. forks) a node
spawnNode :: NtwrkState Node
spawnNode = do
  ntwrk <- get
  let id = ntwrk_nodeC ntwrk
  connEstab <- liftIO $ newChan
  let n = Node {
            n_id = id,
            n_peerConns = [],
            n_newConnChan = connEstab,
            n_maxNeighbours = ntwrk_numNeighbours ntwrk,
            n_ev = ntwrk_admin ntwrk
          }
  liftIO $ forkIO $ evalStateT (runNode $ ntwrk_static ntwrk) n
  put $ ntwrk { ntwrk_nodeC = id + 1 }
  return n

runNode :: [Node] -> NState ()
runNode staticNodes = do
  evChan <- n_ev <$> get
  id <- n_id <$> get
  liftIO $ writeChan evChan $ EvNewNode id
  loop $ do
    liftIO $ threadDelay (1000 * 1000)
    return ()

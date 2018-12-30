module Node where


import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State
import System.Posix.Unistd

import Types
import Util
import Admin (logEvent)

-- Initializes, and starts (i.e. forks) a node
spawnNode :: NtwrkState Node
spawnNode = do
  ntwrk <- get
  let id = ntwrk_nodeC ntwrk
  connEstab <- liftIO $ atomically $ newTChan
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
  id <- n_id <$> get
  evChan <- n_ev <$> get
  liftIO $ logEvent evChan $ EvNewNode id
  loop $ do
    n <- get
    ev <- liftIO $ readAnyFromEitherTChans (map p_connIn $ n_peerConns n)
                                           [n_newConnChan n]
    case ev of
      Left peerMsg -> do
        liftIO $ logEvent evChan $ EvPeerMsgRcv id peerMsg
        return ()
      Right (ConnReq fromId inc resp) -> do
        if length (n_peerConns n) < n_maxNeighbours n
          then do
            out <- liftIO $ atomically $ newTChan
            let peerConn = PeerConn {
                            p_connIn = inc,
                            p_connOut = out
                          }
                flippedConn = PeerConn {
                                p_connIn = out,
                                p_connOut = inc
                              }

            put $ n {n_peerConns = peerConn : n_peerConns n}
            liftIO $ do
              atomically $ writeTChan resp (ConnAcc flippedConn)
              logEvent evChan $ EvNewConn id fromId
          else do
            return ()
      Right (ConnAcc peerConn) -> do
        put $ n {n_peerConns = peerConn : n_peerConns n}
    return ()

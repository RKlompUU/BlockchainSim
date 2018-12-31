module Types where


import Control.Monad.State
import Control.Concurrent.STM.TChan

import Algebra.Graph as G

data Network =
  Network {
    -- ntwrk_admin: administrate events in the network
    --              this effectively builds a global log
    --              from a trace of events
    ntwrk_admin        :: TChan Ev,
    ntwrk_static       :: [Node],
    ntwrk_numNeighbours :: Int,
    ntwrk_nodeC        :: Int
  }

type NodeID = Int

data Node =
  Node {
    n_id            :: NodeID,
    n_peerConns     :: [PeerConn],
    n_newConnChan   :: TChan ConnEstab,
    n_maxNeighbours :: Int,
    n_ev            :: TChan Ev
  }

data PeerConn =
  PeerConn {
    p_connIn :: TChan PeerMsg,
    p_connOut :: TChan PeerMsg
  }

data PeerMsg =
  NewBlock
  deriving Show

data Observer =
  Observer {
    o_graph :: G.Graph NodeID,
    o_ev    :: TChan Ev
  }

data Ev =
    EvNewNode Int
  | EvNewConn Int Int
  | EvPeerMsgRcv Int PeerMsg
  | EvPrintNetwork
  deriving Show

data ConnEstab =
    ConnReq Int (TChan PeerMsg) (TChan ConnEstab)
  | ConnAcc PeerConn

type NtwrkState = StateT Network IO
type NState = StateT Node IO
type OState = StateT Observer IO

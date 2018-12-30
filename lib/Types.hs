module Types where


import Control.Concurrent
import Control.Concurrent.Chan

import Algebra.Graph as G

import Control.Monad.State

data Network =
  Network {
    -- ntwrk_admin: administrate events in the network
    --              this effectively builds a global log
    --              from a trace of events
    ntwrk_admin        :: Chan Ev,
    ntwrk_static       :: [Node],
    ntwrk_numNeighbours :: Int,
    ntwrk_nodeC        :: Int
  }

type NodeID = Int

data Node =
  Node {
    n_id            :: NodeID,
    n_peerConns     :: [Chan PeerMsg],
    n_newConnChan   :: Chan ConnEstab,
    n_maxNeighbours :: Int,
    n_ev            :: Chan Ev
  }

data PeerMsg =
  NewBlock

data Observer =
  Observer {
    o_graph :: G.Graph NodeID,
    o_ev    :: Chan Ev
  }

data Ev =
    EvNewNode Int
  | EvNewConn Int Int
  deriving Show

data ConnEstab =
    ConnReq
  | ConnAcc (Chan PeerMsg)

type NtwrkState = StateT Network IO
type NState = StateT Node IO
type OState = StateT Observer IO

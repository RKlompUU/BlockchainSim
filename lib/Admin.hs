module Admin where


import System.Process

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State

import Algebra.Graph as G
import Algebra.Graph.Export.Dot as G

import Types
import Util

runObserver :: NtwrkState ()
runObserver = do
  evChan <- ntwrk_admin <$> get
  let admin = Observer G.empty evChan
  liftIO $ do
    forkIO $ evalStateT observer admin
    return ()

observer :: OState ()
observer = do
  evChan <- o_ev <$> get
  loop $ do
    ev <- liftIO $ atomically $ readTChan evChan
    liftIO $ putStrLn (show ev)
    case ev of
      EvNewConn n1 n2 -> do
        oSt <- get
        let g' = vertex n1 * vertex n2
               + vertex n2 * vertex n1
               + o_graph oSt
        put $ oSt { o_graph = g' }
      EvPrintNetwork -> do
        g <- o_graph <$> get
        let dotStr = G.export style g
        liftIO $ do
          putStrLn dotStr
          writeFile ".network.dot" dotStr
          callCommand "dot -Tsvg .network.dot > .network.svg"
          spawnCommand "google-chrome .network.svg"
          return ()
      _ -> return ()
    return ()

style :: Style Int String
style =
  Style {
      graphName               = "" -- "Network Structure"
    , preamble                = ["  // P2P network printout", ""]
    , graphAttributes         = [] -- ["label" := "Example"]
    , defaultVertexAttributes = ["shape" := "circle"]
    , defaultEdgeAttributes   = mempty
    , vertexName              = \x   -> "n" ++ show x
    , vertexAttributes        = \x   -> ["color" := "blue"   | odd x      ]
    , edgeAttributes          = \x y -> ["style" := "dashed" | odd (x * y)]
    }

logEvent :: TChan Ev -> Ev -> IO ()
logEvent chan ev =
  atomically $ writeTChan chan ev

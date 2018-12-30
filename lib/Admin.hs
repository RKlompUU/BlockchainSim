module Admin where


import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State

import Algebra.Graph as G

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
      _ -> return ()
    return ()

logEvent :: TChan Ev -> Ev -> IO ()
logEvent chan ev =
  atomically $ writeTChan chan ev

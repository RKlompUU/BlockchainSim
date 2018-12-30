module Util where


import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

loop :: Monad m => m () -> m ()
loop m =
  m >> loop m

for :: Monad m => [a] -> (a -> m b) -> m [b]
for [] _ = return []
for (x:xs) m = do
  y <- m x
  ys <- for xs m
  return $ y : ys

readEitherTChan :: TChan a -> TChan b -> IO (Either a b)
readEitherTChan a b =
  atomically $ readAnyFromEitherTChans' [a] [b]

readAnyTChan :: [TChan a] -> IO a
readAnyTChan chans =
  atomically $ readAnyTChan' chans

readAnyFromEitherTChans :: [TChan a] -> [TChan b] -> IO (Either a b)
readAnyFromEitherTChans [] bs =
  Right <$> readAnyTChan bs
readAnyFromEitherTChans as [] =
  Left <$> readAnyTChan as
readAnyFromEitherTChans as bs =
  atomically $ readAnyFromEitherTChans' as bs

readAnyFromEitherTChans' :: [TChan a] -> [TChan b] -> STM (Either a b)
readAnyFromEitherTChans' a b =
  (Left <$> readAnyTChan' a) `orElse` (Right <$> readAnyTChan' b)

readAnyTChan' :: [TChan a] -> STM a
readAnyTChan' (chan:chans) =
  foldl foldChan (readTChan chan) chans
  where foldChan cont chan = readTChan chan `orElse` cont

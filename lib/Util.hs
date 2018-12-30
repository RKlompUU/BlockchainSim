module Util where

loop :: Monad m => m () -> m ()
loop m =
  m >> loop m

for :: Monad m => [a] -> (a -> m b) -> m [b]
for [] _ = return []
for (x:xs) m = do
  y <- m x
  ys <- for xs m
  return $ y : ys

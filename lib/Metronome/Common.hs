module Metronome.Common where

import           Control.Concurrent
import           Control.Monad      (void)

delaySeconds :: RealFrac n => n -> IO ()
delaySeconds n = threadDelay (round $ 1000000 * n)

-- | Do an action, then (asynchronously) wait the amount of time until the next
-- beat.
bpmWait :: Int -> IO a -> IO ()
bpmWait bpm_ io = do
  threadId <- forkIO (void io)
  delaySeconds (60 / fromIntegral bpm_ :: Double)
  killThread threadId

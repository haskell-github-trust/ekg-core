{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | Perform 100,000 atomic increments using 100 concurrent writers.
module Main where

import Control.Concurrent
import Control.Monad
import qualified System.Metrics.Counter as C

main :: IO ()
main = do
    counter <- C.new
    locks <- replicateM n newEmptyMVar
    mapM_ (forkIO . work counter iters) locks
    mapM_ takeMVar locks
    fin <- C.read counter
    when (fin /= fromIntegral (n * iters)) $
        fail "Data.Atomic is broken!"
  where
    n = 100
    iters = 100000

    work :: C.Counter -> Int -> MVar () -> IO ()
    work !_ 0 !lock     = putMVar lock ()
    work counter i lock = C.inc counter >> work counter (i - 1) lock

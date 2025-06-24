{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

#include "distrib.h"
#include "MachDeps.h"

-- | This module defines a type for tracking statistics about a series
-- of events. An event could be handling of a request and the value
-- associated with the event -- the value you'd pass to 'add' -- could
-- be the amount of time spent serving that request (e.g. in
-- milliseconds). All operations are thread safe.
module System.Metrics.Distribution
    ( Distribution
    , new
    , add
    , addN
    , read

      -- * Gathered statistics
    , Stats
    , mean
    , variance
    , count
    , sum
    , min
    , max
    ) where

import Control.Monad (forM_, replicateM)
import Prelude hiding (max, min, read, sum)

import Foreign.Storable (sizeOf)

import GHC.Float
import GHC.Int (Int(..), Int64(..))
import GHC.IO
import GHC.Prim

import Data.Array
import System.Metrics.Distribution.Internal (Stats(..))
import System.Metrics.ThreadId

-- Three cases for these definitions
-- 64-bit, GHC has Int64
-- 64-bit, GHC doesn't have Int64
-- 32-bit

-- 64-bit machine, Int ~ Int64, do it the fast way:
#if SIZEOF_HSINT == 8

#if MIN_VERSION_base(4,17,0)
int64ToDouble :: Int64## -> Double##
int64ToDouble i = int2Double## (int64ToInt## i)

intToInt64 :: Int## -> Int64##
intToInt64 = intToInt64##

plusInt64 :: Int64## -> Int64## -> Int64##
plusInt64 = plusInt64##

eqInt64 :: Int64## -> Int64## -> Int##
eqInt64 = eqInt64##

readInt64Array :: MutableByteArray## d -> Int## -> State## d -> (## State## d, Int64## ##)
readInt64Array = readInt64Array##

writeInt64Array :: MutableByteArray## d -> Int## -> Int64## -> State## d -> State## d
writeInt64Array = writeInt64Array##

#else
int64ToDouble :: Int## -> Double##
int64ToDouble = int2Double##

intToInt64 :: Int## -> Int##
intToInt64 i = i

plusInt64 :: Int## -> Int## -> Int##
plusInt64 = (+##)

eqInt64 :: Int## -> Int## -> Int##
eqInt64 = (==##)

readInt64Array :: MutableByteArray## d -> Int## -> State## d -> (## State## d, Int## ##)
readInt64Array = readIntArray##

writeInt64Array :: MutableByteArray## d -> Int## -> Int## -> State## d -> State## d
writeInt64Array = writeIntArray##
#endif

#else
-- NB: I've only tested these with the WASM backend:
 
intToInt64 :: Int## -> Int64##
intToInt64 = intToInt64##

plusInt64 :: Int64## -> Int64## -> Int64##
plusInt64 = plusInt64##

eqInt64 :: Int64## -> Int64## -> Int##
eqInt64 = eqInt64##

readInt64Array :: MutableByteArray## d -> Int## -> State## d -> (## State## d, Int64## ##)
readInt64Array = readInt64Array##

writeInt64Array :: MutableByteArray## d -> Int## -> Int64## -> State## d -> State## d
writeInt64Array = writeInt64Array##

-- I don't know a better way on 32-bit machines...
int64ToDouble :: Int64## -> Double##
int64ToDouble i =
    case fromIntegral (I64## i) of (D## d) -> d
#endif

-- | An metric for tracking events.
newtype Distribution = Distribution { unD :: Array Stripe }

newtype Stripe = Stripe { stripeD :: Distrib }

data Distrib = Distrib (MutableByteArray## RealWorld)

unI :: Int -> Int##
unI (I## x) = x
{-# INLINE unI #-}

distribLen :: Int
distribLen = (#size struct distrib)

lockPos :: Int
lockPos = div (#offset struct distrib, lock)
              (sizeOf (undefined :: Int))

countPos :: Int
countPos = div (#offset struct distrib, count)
               (sizeOf (undefined :: Int64))

meanPos :: Int
meanPos = div (#offset struct distrib, mean)
              (sizeOf (undefined :: Double))

sumSqDeltaPos :: Int
sumSqDeltaPos = div (#offset struct distrib, sum_sq_delta)
                    (sizeOf (undefined :: Double))

sumPos :: Int
sumPos = div (#offset struct distrib, sum)
             (sizeOf (undefined :: Double))

minPos :: Int
minPos = div (#offset struct distrib, min)
             (sizeOf (undefined :: Double))

maxPos :: Int
maxPos = div (#offset struct distrib, max)
             (sizeOf (undefined :: Double))

newDistrib :: IO Distrib
newDistrib = IO $ \s ->
    case newByteArray## (unI distribLen) s of { (## s1, mba ##) ->
    -- probably unnecessary
    case atomicWriteIntArray## mba (unI lockPos) 0## s1 of { s2 ->
    case writeInt64Array mba (unI countPos) (intToInt64 0##) s2 of { s3 ->
    case writeDoubleArray## mba (unI meanPos) 0.0#### s3 of { s4 ->
    case writeDoubleArray## mba (unI sumSqDeltaPos) 0.0#### s4 of { s5 ->
    case writeDoubleArray## mba (unI sumPos) 0.0#### s5 of { s6 ->
    case writeDoubleArray## mba (unI minPos) 0.0#### s6 of { s7 ->
    case writeDoubleArray## mba (unI maxPos) 0.0#### s7 of { s8 ->
    (## s8, Distrib mba ##) }}}}}}}}

newStripe :: IO Stripe
newStripe = do
    d <- newDistrib
    return $! Stripe
        { stripeD = d
        }

-- | Number of lock stripes. Should be greater or equal to the number
-- of HECs.
numStripes :: Int
numStripes = 8

-- | Get the stripe to use for this thread.
myStripe :: Distribution -> IO Stripe
myStripe distrib = do
    tid <- myCapability
    return $! unD distrib `index` (tid `mod` numStripes)

------------------------------------------------------------------------
-- Exposed API

-- | Create a new distribution.
new :: IO Distribution
new = (Distribution . fromList numStripes) `fmap`
      replicateM numStripes newStripe

-- | Add a value to the distribution.
add :: Distribution -> Double -> IO ()
add distrib val = addN distrib val 1

{-# INLINE spinLock #-}
spinLock :: MutableByteArray## RealWorld -> State## RealWorld -> State## RealWorld
spinLock mba = \s ->
    case casIntArray## mba (unI lockPos) 0## 1## s of { (## s1, r ##) ->
    case r of { 0## -> s1; _ ->
    case yield## s1 of { s2 ->
    spinLock mba s2 }}}

{-# INLINE spinUnlock #-}
spinUnlock :: MutableByteArray## RealWorld -> State## RealWorld -> State## RealWorld
spinUnlock mba = \s ->
    case writeIntArray## mba (unI lockPos) 0## s of { s2 -> s2 }


-- | Add the same value to the distribution N times.
--   Mean and variance are computed according to
--   http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm
addN :: Distribution -> Double -> Int64 -> IO ()
addN distribution (D## val) (I64## n) = IO $ \s ->
    case myStripe distribution of { (IO myStripe') ->
    case myStripe' s of { (## s1, (Stripe (Distrib mba)) ##) ->
    case spinLock mba s1 of { s2 ->
    case readInt64Array mba (unI countPos) s2 of { (## s3, count ##) ->
    case readDoubleArray## mba (unI meanPos) s3 of { (## s4, mean ##) ->
    case readDoubleArray## mba (unI sumSqDeltaPos) s4 of { (## s5, sumSqDelta ##) ->
    case readDoubleArray## mba (unI sumPos) s5 of { (## s6, dSum ##) ->
    case readDoubleArray## mba (unI minPos) s6 of { (## s7, dMin ##) ->
    case readDoubleArray## mba (unI maxPos) s7 of { (## s8, dMax ##) ->
    case plusInt64 count n of { count' ->
    case val -#### mean of { delta ->
    case mean +#### ((int64ToDouble n) *#### delta /#### (int64ToDouble count')) of { mean' ->
    case sumSqDelta +#### (delta *#### (val -#### mean') *#### (int64ToDouble n)) of { sumSqDelta' ->
    case writeInt64Array mba (unI countPos) count' s8 of { s9 ->
    case writeDoubleArray## mba (unI meanPos) mean' s9 of { s10 ->
    case writeDoubleArray## mba (unI sumSqDeltaPos) sumSqDelta' s10 of { s11 ->
    case writeDoubleArray## mba (unI sumPos) (dSum +#### val) s11 of { s12 ->
    case (case val <#### dMin of { 0## -> dMin; _ -> val }) of { dMin' ->
    case (case val >#### dMax of { 0## -> dMax; _ -> val }) of { dMax' ->
    case writeDoubleArray## mba (unI minPos) dMin' s12 of { s13 ->
    case writeDoubleArray## mba (unI maxPos) dMax' s13 of { s14 ->
    case spinUnlock mba s14 of { s15 ->
    (## s15, () ##) }}}}}}}}}}}}}}}}}}}}}}

-- | Combine 'b' with 'a', writing the result in 'a'. Takes the lock of
--   'b' while combining, but doesn't otherwise modify 'b'. 'a' is
--   assumed to not be used concurrently.
--   See also:
--   http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
combine :: Distrib -> Distrib -> IO ()
combine (Distrib bMBA) (Distrib aMBA) = IO $ \s ->
    case spinLock bMBA s of { s1 ->
    case readInt64Array aMBA (unI countPos) s1 of { (## s2, aCount ##) ->
    case readInt64Array bMBA (unI countPos) s2 of { (## s3, bCount ##) ->
    case plusInt64 aCount bCount of { count' ->
    case readDoubleArray## aMBA (unI meanPos) s3 of { (## s4, aMean ##) ->
    case readDoubleArray## bMBA (unI meanPos) s4 of { (## s5, bMean ##) ->
    case bMean -#### aMean of { delta ->
    case (   (((int64ToDouble aCount) *#### aMean) +#### ((int64ToDouble bCount) *#### bMean))
         /#### (int64ToDouble count')
         ) of { mean' ->
    case readDoubleArray## aMBA (unI sumSqDeltaPos) s5 of { (## s6, aSumSqDelta ##) ->
    case readDoubleArray## bMBA (unI sumSqDeltaPos) s6 of { (## s7, bSumSqDelta ##) ->
    case (   aSumSqDelta 
         +#### bSumSqDelta
         +#### (   delta
               *#### delta
               *#### (   (int64ToDouble aCount) *#### (int64ToDouble bCount)
                     /#### (int64ToDouble count')
                     )
               )
         ) of { sumSqDelta' ->
    case writeInt64Array aMBA (unI countPos) count' s7 of { s8 ->
    case (case eqInt64 count' (intToInt64 0##) of { 0## -> mean'; _ -> 0.0#### }) of { writeMean ->
    case writeDoubleArray## aMBA (unI meanPos) writeMean s8 of { s9 ->
    case writeDoubleArray## aMBA (unI sumSqDeltaPos) sumSqDelta' s9 of { s10 ->
    case readDoubleArray## aMBA (unI sumPos) s10 of { (## s11, aSum ##) ->
    case readDoubleArray## bMBA (unI sumPos) s11 of { (## s12, bSum ##) ->
    case writeDoubleArray## aMBA (unI sumPos) (aSum +#### bSum) s12 of { s13 ->
    case readDoubleArray## bMBA (unI minPos) s13 of { (## s14, bMin ##) ->
    case writeDoubleArray## aMBA (unI minPos) bMin s14 of { s15 ->
    -- This is slightly hacky, but ok: see
    -- 813aa426be78e8abcf1c7cdd43433bcffa07828e
    case readDoubleArray## bMBA (unI maxPos) s15 of { (## s16, bMax ##) ->
    case writeDoubleArray## aMBA (unI maxPos) bMax s16 of { s17 ->
    case spinUnlock bMBA s17 of { s18 ->
    (## s18, () ##) }}}}}}}}}}}}}}}}}}}}}}}

-- | Get the current statistical summary for the event being tracked.
read :: Distribution -> IO Stats
read distrib = do
    result@(Distrib mba) <- newDistrib
    forM_ (toList $ unD distrib) $ \(Stripe d) ->
        combine d result
    IO $ \s ->
        case readInt64Array mba (unI countPos) s of { (## s1, count ##) ->
        case readDoubleArray## mba (unI meanPos) s1 of { (## s2, mean ##) ->
        case readDoubleArray## mba (unI sumSqDeltaPos) s2 of { (## s3, sumSqDelta ##) ->
        case readDoubleArray## mba (unI sumPos) s3 of { (## s4, dSum ##) ->
        case readDoubleArray## mba (unI minPos) s4 of { (## s5, dMin ##) ->
        case readDoubleArray## mba (unI maxPos) s5 of { (## s6, dMax ##) ->
        (## s6
        , Stats { mean = (D## mean)
                , variance = if (I64## count) == 0 then 0.0
                             else (D## sumSqDelta) / (D## (int64ToDouble count))
                , count = (I64## count)
                , sum = (D## dSum)
                , min = (D## dMin)
                , max = (D## dMax)
                }
        ##) }}}}}}

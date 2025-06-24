{-# LANGUAGE BangPatterns
           , CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           #-}
-- | An atomic integer value. All operations are thread safe.
module Data.Atomic
    (
      Atomic
    , new
    , read
    , write
    , inc
    , dec
    , add
    , subtract
    ) where

#include "MachDeps.h"
#ifndef SIZEOF_HSINT
#error "MachDeps.h didn't define SIZEOF_HSINT"
#endif

import Prelude hiding (read, subtract)

import GHC.Int

#if SIZEOF_HSINT == 8

-- 64-bit imports
import GHC.IO
import GHC.Prim

#else

-- 32-bit imports
import Data.IORef

#endif


-- 64-bit machine, Int ~ Int64, do it the fast way:
#if SIZEOF_HSINT == 8

#if MIN_VERSION_base(4,17,0)
int64ToInt :: Int64# -> Int#
int64ToInt = int64ToInt#

intToInt64 :: Int# -> Int64#
intToInt64 = intToInt64#
#else
int64ToInt :: Int# -> Int#
int64ToInt i = i

intToInt64 :: Int# -> Int#
intToInt64 i = i
#endif

-- | A mutable, atomic integer.
data Atomic = C (MutableByteArray# RealWorld)

-- | Create a new, zero initialized, atomic.
new :: Int64 -> IO Atomic
new (I64# n64) = IO $ \s ->
    case newByteArray# SIZEOF_HSINT# s of { (# s1, mba #) ->
    case atomicWriteIntArray# mba 0# (int64ToInt n64) s1 of { s2 ->
    (# s2, C mba #) }}

read :: Atomic -> IO Int64
read (C mba) = IO $ \s ->
    case atomicReadIntArray# mba 0# s of { (# s1, n #) ->
    (# s1, I64# (intToInt64 n) #)}

-- | Set the atomic to the given value.
write :: Atomic -> Int64 -> IO ()
write (C mba) (I64# n64) = IO $ \s ->
    case atomicWriteIntArray# mba 0# (int64ToInt n64) s of { s1 ->
    (# s1, () #) }

-- | Increase the atomic by the given amount.
add :: Atomic -> Int64 -> IO ()
add (C mba) (I64# n64) = IO $ \s ->
    case fetchAddIntArray# mba 0# (int64ToInt n64) s of { (# s1, _ #) ->
    (# s1, () #) }

-- | Decrease the atomic by the given amount.
subtract :: Atomic -> Int64 -> IO ()
subtract (C mba) (I64# n64) = IO $ \s ->
    case fetchSubIntArray# mba 0# (int64ToInt n64) s of { (# s1, _ #) ->
    (# s1, () #) }

#else

-- 32-bit machine, Int ~ Int32, fall back to IORef. This could be replaced with
-- faster implementations for specific 32-bit machines in the future, but the
-- idea is to preserve 64-bit width for counters.

newtype Atomic = C (IORef Int64)

-- | Create a new, zero initialized, atomic.
new :: Int64 -> IO Atomic
new = fmap C . newIORef

read :: Atomic -> IO Int64
read (C ior) = readIORef ior

-- | Set the atomic to the given value.
write :: Atomic -> Int64 -> IO ()
write (C ior) !i = atomicWriteIORef ior i

-- | Increase the atomic by the given amount.
add :: Atomic -> Int64 -> IO ()
add (C ior) !i = atomicModifyIORef' ior (\(!n) -> (n+i, ()))

-- | Decrease the atomic by the given amount.
subtract :: Atomic -> Int64 -> IO ()
subtract (C ior) !i = atomicModifyIORef' ior (\(!n) -> (n-i, ()))

#endif

-- | Increase the atomic by one.
inc :: Atomic -> IO ()
inc atomic = add atomic 1

-- | Decrease the atomic by one.
dec :: Atomic -> IO ()
dec atomic = subtract atomic 1

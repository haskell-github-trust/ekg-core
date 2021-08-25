{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | A module for defining metrics that can be monitored.
--
-- Metrics are used to monitor program behavior and performance. All
-- metrics have
--
--  * a name, and
--
--  * a way to get the metric's current value.
--
-- This module provides a way to register metrics in a global \"metric
-- store\". The store can then be used to get a snapshot of all
-- metrics. The store also serves as a central place to keep track of
-- all the program's metrics, both user and library defined.
--
-- Here's an example of creating a single counter, used to count the
-- number of request served by a web server:
--
-- > import System.Metrics
-- > import qualified System.Metrics.Counter as Counter
-- >
-- > main = do
-- >     store <- newStore
-- >     requests <- createCounter "myapp.request_count" store
-- >     -- Every time we receive a request:
-- >     Counter.inc requests
--
-- This module also provides a way to register a number of predefined
-- metrics that are useful in most applications. See e.g.
-- 'registerGcMetrics'.
module System.Metrics
    (
      -- * Naming metrics
      -- $naming

      -- * The metric store
      -- $metric-store
      Store
    , newStore

      -- * Registering metrics
      -- $registering
    , registerCounter
    , registerGauge
    , registerLabel
    , registerDistribution
    , registerGroup

      -- ** Convenience functions
      -- $convenience
    , createCounter
    , createGauge
    , createLabel
    , createDistribution

      -- ** Predefined metrics
      -- $predefined
    , registerGcMetrics

      -- * Sampling metrics
      -- $sampling
    , Sample
    , sampleAll
    , Value(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified GHC.Stats as Stats
import Prelude hiding (read)

import System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import System.Metrics.Distribution (Distribution)
import qualified System.Metrics.Distribution as Distribution
import System.Metrics.Gauge (Gauge)
import qualified System.Metrics.Gauge as Gauge
import System.Metrics.Label (Label)
import qualified System.Metrics.Label as Label

-- $naming
-- Compound metric names should be separated using underscores.
-- Example: @request_count@. Periods in the name imply namespacing.
-- Example: @\"myapp.users\"@. Some consumers of metrics will use
-- these namespaces to group metrics in e.g. UIs.
--
-- Libraries and frameworks that want to register their own metrics
-- should prefix them with a namespace, to avoid collision with
-- user-defined metrics and metrics defined by other libraries. For
-- example, the Snap web framework could prefix all its metrics with
-- @\"snap.\"@.
--
-- It's customary to suffix the metric name with a short string
-- explaining the metric's type e.g. using @\"_ms\"@ to denote
-- milliseconds.

------------------------------------------------------------------------
-- * The metric store

-- $metric-store
-- The metric store is a shared store of metrics. It allows several
-- disjoint components (e.g. libraries) to contribute to the set of
-- metrics exposed by an application. Libraries that want to provide a
-- set of metrics should defined a register method, in the style of
-- 'registerGcMetrics', that registers the metrics in the 'Store'. The
-- register function should document which metrics are registered and
-- their types (i.e. counter, gauge, label, or distribution).

-- | A mutable metric store.
newtype Store = Store { storeState :: IORef State }

type GroupId = Int

-- | The 'Store' state.
data State = State
     { stateMetrics :: !(M.HashMap T.Text (Either MetricSampler GroupId))
     , stateGroups  :: !(IM.IntMap GroupSampler)
     , stateNextId  :: {-# UNPACK #-} !Int
     }

data GroupSampler = forall a. GroupSampler
     { groupSampleAction   :: !(IO a)
     , groupSamplerMetrics :: !(M.HashMap T.Text (a -> Value))
     }

-- TODO: Rename this to Metric and Metric to SampledMetric.
data MetricSampler = CounterS !(IO Int64)
                   | GaugeS !(IO Int64)
                   | LabelS !(IO T.Text)
                   | DistributionS !(IO Distribution.Stats)

-- | Create a new, empty metric store.
newStore :: IO Store
newStore = do
    state <- newIORef $ State M.empty IM.empty 0
    return $ Store state

------------------------------------------------------------------------
-- * Registering metrics

-- $registering
-- Before metrics can be sampled they need to be registered with the
-- metric store. The same metric name can only be used once. Passing a
-- metric name that has already been used to one of the register
-- function is an 'error'.

-- | Register a non-negative, monotonically increasing, integer-valued
-- metric. The provided action to read the value must be thread-safe.
-- Also see 'createCounter'.
registerCounter :: T.Text    -- ^ Counter name
                -> IO Int64  -- ^ Action to read the current metric value
                -> Store     -- ^ Metric store
                -> IO ()
registerCounter name sample store =
    register name (CounterS sample) store

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe. Also see 'createGauge'.
registerGauge :: T.Text    -- ^ Gauge name
              -> IO Int64  -- ^ Action to read the current metric value
              -> Store     -- ^ Metric store
              -> IO ()
registerGauge name sample store =
    register name (GaugeS sample) store

-- | Register a text metric. The provided action to read the value
-- must be thread-safe. Also see 'createLabel'.
registerLabel :: T.Text     -- ^ Label name
              -> IO T.Text  -- ^ Action to read the current metric value
              -> Store      -- ^ Metric store
              -> IO ()
registerLabel name sample store =
    register name (LabelS sample) store

-- | Register a distribution metric. The provided action to read the
-- value must be thread-safe. Also see 'createDistribution'.
registerDistribution
    :: T.Text                 -- ^ Distribution name
    -> IO Distribution.Stats  -- ^ Action to read the current metric
                              -- value
    -> Store                  -- ^ Metric store
    -> IO ()
registerDistribution name sample store =
    register name (DistributionS sample) store

register :: T.Text
         -> MetricSampler
         -> Store
         -> IO ()
register name sample store = do
    atomicModifyIORef (storeState store) $ \ state@State{..} ->
        case M.member name stateMetrics of
            False -> let !state' = state {
                               stateMetrics = M.insert name
                                              (Left sample)
                                              stateMetrics
                             }
                     in (state', ())
            True  -> alreadyInUseError name

-- | Raise an exception indicating that the metric name is already in
-- use.
alreadyInUseError :: T.Text -> a
alreadyInUseError name =
    error $ "The name \"" ++ show name ++ "\" is already taken " ++
    "by a metric."

-- | Register an action that will be executed any time one of the
-- metrics computed from the value it returns needs to be sampled.
--
-- When one or more of the metrics listed in the first argument needs
-- to be sampled, the action is executed and the provided getter
-- functions will be used to extract the metric(s) from the action's
-- return value.
--
-- The registered action might be called from a different thread and
-- therefore needs to be thread-safe.
--
-- This function allows you to sample groups of metrics together. This
-- is useful if
--
-- * you need a consistent view of several metric or
--
-- * sampling the metrics together is more efficient.
--
-- For example, sampling GC statistics needs to be done atomically or
-- a GC might strike in the middle of sampling, rendering the values
-- incoherent. Sampling GC statistics is also more efficient if done
-- in \"bulk\", as the run-time system provides a function to sample all
-- GC statistics at once.
--
-- Note that sampling of the metrics is only atomic if the provided
-- action computes @a@ atomically (e.g. if @a@ is a record, the action
-- needs to compute its fields atomically if the sampling is to be
-- atomic.)
--
-- Example usage:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import qualified Data.HashMap.Strict as M
-- > import GHC.Stats
-- > import System.Metrics
-- >
-- > main = do
-- >     store <- newStore
-- >     let metrics =
-- >             [ ("num_gcs", Counter . numGcs)
-- >             , ("max_bytes_used", Gauge . maxBytesUsed)
-- >             ]
-- >     registerGroup (M.fromList metrics) getGCStats store
registerGroup
    :: M.HashMap T.Text
       (a -> Value)  -- ^ Metric names and getter functions.
    -> IO a          -- ^ Action to sample the metric group
    -> Store         -- ^ Metric store
    -> IO ()
registerGroup getters cb store = do
    atomicModifyIORef (storeState store) $ \ State{..} ->
        let !state' = State
                { stateMetrics = M.foldlWithKey' (register_ stateNextId)
                                 stateMetrics getters
                , stateGroups  = IM.insert stateNextId
                                 (GroupSampler cb getters)
                                 stateGroups
                , stateNextId  = stateNextId + 1
                }
       in (state', ())
  where
    register_ groupId metrics name _ = case M.lookup name metrics of
        Nothing -> M.insert name (Right groupId) metrics
        Just _  -> alreadyInUseError name

------------------------------------------------------------------------
-- ** Convenience functions

-- $convenience
-- These functions combined the creation of a mutable reference (e.g.
-- a 'Counter') with registering that reference in the store in one
-- convenient function.

-- | Create and register a zero-initialized counter.
createCounter :: T.Text  -- ^ Counter name
              -> Store   -- ^ Metric store
              -> IO Counter
createCounter name store = do
    counter <- Counter.new
    registerCounter name (Counter.read counter) store
    return counter

-- | Create and register a zero-initialized gauge.
createGauge :: T.Text  -- ^ Gauge name
            -> Store   -- ^ Metric store
            -> IO Gauge
createGauge name store = do
    gauge <- Gauge.new
    registerGauge name (Gauge.read gauge) store
    return gauge

-- | Create and register an empty label.
createLabel :: T.Text  -- ^ Label name
            -> Store   -- ^ Metric store
            -> IO Label
createLabel name store = do
    label <- Label.new
    registerLabel name (Label.read label) store
    return label

-- | Create and register an event tracker.
createDistribution :: T.Text  -- ^ Distribution name
                   -> Store   -- ^ Metric store
                   -> IO Distribution
createDistribution name store = do
    event <- Distribution.new
    registerDistribution name (Distribution.read event) store
    return event

------------------------------------------------------------------------
-- * Predefined metrics

-- $predefined
-- This library provides a number of pre-defined metrics that can
-- easily be added to a metrics store by calling their register
-- function.

#if MIN_VERSION_base(4,10,0)
#else
-- | Convert seconds to nanoseconds.
sToNs :: Double -> Int64
sToNs s = round (s * 1000000000.0)
#endif

-- | Register a number of metrics related to garbage collector
-- behavior.
--
-- To enable GC statistics collection, either run your program with
--
-- > +RTS -T
--
-- or compile it with
--
-- > -with-rtsopts=-T
--
-- The runtime overhead of @-T@ is very small so it's safe to always
-- leave it enabled.
--
-- Registered counters (see also "GHC.Stats"):
--
-- > [@rts.gcs@] - Total number of GCs
-- > [@rts.major_gcs@] - Total number of major (oldest generation) GCs
-- > [@rts.allocated_bytes@] - Total bytes allocated
-- > [@rts.max_live_bytes@] - Maximum live data (including large objects + compact regions) in the heap. Updated after a major GC.
-- > [@rts.max_large_objects_bytes@] - Maximum live data in large objects
-- > [@rts.max_compact_bytes@] - Maximum live data in compact regions
-- > [@rts.max_slop_bytes@] - Maximum slop
-- > [@rts.max_mem_in_use_bytes@] - Maximum memory in use by the RTS
-- > [@rts.cumulative_live_bytes@] - Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program.
-- > [@rts.copied_bytes@] - Sum of copied_bytes across all GCs
-- > [@rts.par_copied_bytes@] - Sum of copied_bytes across all parallel GCs
-- > [@rts.cumulative_par_max_copied_bytes@] - Sum of par_max_copied_bytes across all parallel GCs. Deprecated.
-- > [@rts.cumulative_par_balanced_copied_bytes@] - Sum of par_balanced_copied bytes across all parallel GCs
-- > [@rts.init_cpu_ns@] - Total CPU time used by the init phase @since 4.12.0.0
-- > [@rts.init_elapsed_ns@] - Total elapsed time used by the init phase @since 4.12.0.0
-- > [@rts.mutator_cpu_ns@] - Total CPU time used by the mutator
-- > [@rts.mutator_elapsed_ns@] - Total elapsed time used by the mutator
-- > [@rts.gc_cpu_ns@] - Total CPU time used by the GC
-- > [@rts.gc_elapsed_ns@] - Total elapsed time used by the GC
-- > [@rts.cpu_ns@] - Total CPU time (at the previous GC)
-- > [@rts.elapsed_ns@] - Total elapsed time (at the previous GC)
-- > [@rts.gc.gen@] - The generation number of this GC
-- > [@rts.gc.threads@] - Number of threads used in this GC
-- > [@rts.gc.allocated_bytes@] - Number of bytes allocated since the previous GC
-- > [@rts.gc.live_bytes@] - Total amount of live data in the heap (incliudes large + compact data). Updated after every GC. Data in uncollected generations (in minor GCs) are considered live.
-- > [@rts.gc.large_objects_bytes@] - Total amount of live data in large objects
-- > [@rts.gc.compact_bytes@] - Total amount of live data in compact regions
-- > [@rts.gc.slop_bytes@] - Total amount of slop (wasted memory)
-- > [@rts.gc.mem_in_use_bytes@] - Total amount of memory in use by the RTS
-- > [@rts.gc.copied_bytes@] - Total amount of data copied during this GC
-- > [@rts.gc.par_max_copied_bytes@] - In parallel GC, the max amount of data copied by any one thread. Deprecated.
-- > [@rts.gc.sync_elapsed_ns@] - The time elapsed during synchronisation before GC
-- > [@rts.gc.cpu_ns@] - The CPU time used during GC itself
-- > [@rts.gc.elapsed_ns@] - The time elapsed during GC itself
registerGcMetrics :: Store -> IO ()
registerGcMetrics store =
    registerGroup
#if MIN_VERSION_base(4,10,0)
    (M.fromList
     -- We order them the same way as they are in GHC.Stats for easy comparison.
     [ ("rts.gcs"                                    , Counter . fromIntegral . Stats.gcs)
     , ("rts.major_gcs"                              , Counter . fromIntegral . Stats.major_gcs)
     , ("rts.allocated_bytes"                        , Counter . fromIntegral . Stats.allocated_bytes)
     , ("rts.max_live_bytes"                         , Gauge . fromIntegral . Stats.max_live_bytes)
     , ("rts.max_large_objects_bytes"                , Gauge . fromIntegral . Stats.max_large_objects_bytes)
     , ("rts.max_compact_bytes"                      , Gauge . fromIntegral . Stats.max_compact_bytes)
     , ("rts.max_slop_bytes"                         , Gauge . fromIntegral . Stats.max_slop_bytes)
     , ("rts.max_mem_in_use_bytes"                   , Gauge . fromIntegral . Stats.max_mem_in_use_bytes)
     , ("rts.cumulative_live_bytes"                  , Counter . fromIntegral . Stats.cumulative_live_bytes)
     , ("rts.copied_bytes"                           , Counter . fromIntegral . Stats.copied_bytes)
     , ("rts.par_copied_bytes"                       , Gauge . fromIntegral . Stats.par_copied_bytes)
     , ("rts.cumulative_par_max_copied_bytes"        , Gauge . fromIntegral . Stats.cumulative_par_max_copied_bytes)
#if MIN_VERSION_base(4,11,0)
     , ("rts.cumulative_par_balanced_copied_bytes"   , Gauge . fromIntegral . Stats.cumulative_par_balanced_copied_bytes)
#endif
#if MIN_VERSION_base(4,12,0)
     , ("rts.init_cpu_ns"                            , Counter . Stats.init_cpu_ns)
     , ("rts.init_elapsed_ns"                        , Counter . Stats.init_elapsed_ns)
#endif
     , ("rts.mutator_cpu_ns"                         , Counter . Stats.mutator_cpu_ns)
     , ("rts.mutator_elapsed_ns"                     , Counter . Stats.mutator_elapsed_ns)
     , ("rts.gc_cpu_ns"                              , Counter . Stats.gc_cpu_ns)
     , ("rts.gc_elapsed_ns"                          , Counter . Stats.gc_elapsed_ns)
     , ("rts.cpu_ns"                                 , Counter . Stats.cpu_ns)
     , ("rts.elapsed_ns"                             , Counter . Stats.elapsed_ns)
     -- GCDetails
     , ("rts.gc.gen"                                 , Gauge . fromIntegral . Stats.gcdetails_gen . Stats.gc)
     , ("rts.gc.threads"                             , Gauge . fromIntegral . Stats.gcdetails_threads . Stats.gc)
     , ("rts.gc.allocated_bytes"                     , Gauge . fromIntegral . Stats.gcdetails_allocated_bytes . Stats.gc)
     , ("rts.gc.live_bytes"                          , Gauge . fromIntegral . Stats.gcdetails_live_bytes . Stats.gc)
     , ("rts.gc.large_objects_bytes"                 , Gauge . fromIntegral . Stats.gcdetails_large_objects_bytes . Stats.gc)
     , ("rts.gc.compact_bytes"                       , Gauge . fromIntegral . Stats.gcdetails_compact_bytes . Stats.gc)
     , ("rts.gc.slop_bytes"                          , Gauge . fromIntegral . Stats.gcdetails_slop_bytes . Stats.gc)
     , ("rts.gc.mem_in_use_bytes"                    , Gauge . fromIntegral . Stats.gcdetails_mem_in_use_bytes . Stats.gc)
     , ("rts.gc.copied_bytes"                        , Gauge . fromIntegral . Stats.gcdetails_copied_bytes . Stats.gc)
     , ("rts.gc.par_max_copied_bytes"                , Gauge . fromIntegral . Stats.gcdetails_par_max_copied_bytes . Stats.gc)
#if MIN_VERSION_base(4,11,0)
     , ("rts.gc.gcdetails_par_balanced_copied_bytes" , Gauge . fromIntegral . Stats.gcdetails_par_balanced_copied_bytes . Stats.gc)
#endif
     , ("rts.gc.sync_elapsed_ns"                     , Gauge . fromIntegral . Stats.gcdetails_sync_elapsed_ns . Stats.gc)
     , ("rts.gc.cpu_ns"                              , Gauge . fromIntegral . Stats.gcdetails_cpu_ns . Stats.gc)
     , ("rts.gc.elapsed_ns"                          , Gauge . fromIntegral . Stats.gcdetails_elapsed_ns . Stats.gc)
     ])
    getRTSStats
#else
    -- For pre-base-4.10 we translate the names from before GHC commit
    -- 24e6594cc7890babe69b8ba122d171affabad2d1 to their newer equivalents
    -- so that ekg-core always presents the same names (given that e.g.
    -- the ekg Javascript expects them to exist).
    -- The mapping is obtained obtained from
    --   https://hackage.haskell.org/package/base-4.10.0.0/docs/GHC-Stats.html
    -- which has both the old and the new interface, as well as from
    -- the commit diff implementation in `Stats.c`.
    (M.fromList
     [ ("rts.allocated_bytes"         , Counter . Stats.bytesAllocated)
     , ("rts.gcs"                     , Counter . Stats.numGcs)
     , ("rts.major_gcs"               , Counter . Stats.numByteUsageSamples)
     , ("rts.cumulative_live_bytes"   , Counter . Stats.cumulativeBytesUsed)
     , ("rts.copied_bytes"            , Counter . Stats.bytesCopied)
     , ("rts.mutator_cpu_ns"          , Counter . sToNs . Stats.mutatorCpuSeconds)
     , ("rts.mutator_elapsed_ns"      , Counter . sToNs . Stats.mutatorWallSeconds)
     , ("rts.gc_cpu_ns"               , Counter . sToNs . Stats.gcCpuSeconds)
     , ("rts.gc_elapsed_ns"           , Counter . sToNs . Stats.gcWallSeconds)
     , ("rts.cpu_ns"                  , Counter . sToNs . Stats.cpuSeconds)
     , ("rts.elapsed_ns"              , Counter . sToNs . Stats.wallSeconds)
     , ("rts.max_live_bytes"          , Gauge . Stats.maxBytesUsed)
     , ("rts.gc.live_bytes"           , Gauge . Stats.currentBytesUsed)
     , ("rts.gc.slop_bytes"           , Gauge . Stats.currentBytesSlop)
     , ("rts.max_slop_bytes"          , Gauge . Stats.maxBytesSlop)
     , ("rts.max_mem_in_use_bytes"    , Gauge . Stats.peakMegabytesAllocated)
     -- Note that historically, the values
     --     par_tot_bytes_copied were both
     --     par_avg_bytes_copied
     -- were both taken from
     --     gcParTotBytesCopied
     -- after `parAvgBytesCopied` was renamed to `gcParTotBytesCopied`;
     -- see `ekg` commit
     --     27467a61 - parAvgBytesCopied was renamed in GHC 7.6.1
     , ("rts.par_copied_bytes"        , Gauge . gcParTotBytesCopied)
     , ("rts.gc.par_max_copied_bytes" , Gauge . Stats.parMaxBytesCopied)
     ])
    getGcStats
#endif
    store

#if MIN_VERSION_base(4,10,0)
-- | Get RTS statistics.
getRTSStats :: IO Stats.RTSStats
getRTSStats = do
    enabled <- Stats.getRTSStatsEnabled
    if enabled
        then Stats.getRTSStats
        else return emptyRTSStats

-- | Empty RTS statistics, as if the application hasn't started yet.
emptyRTSStats :: Stats.RTSStats
emptyRTSStats = Stats.RTSStats
    { gcs                                  = 0
    , major_gcs                            = 0
    , allocated_bytes                      = 0
    , max_live_bytes                       = 0
    , max_large_objects_bytes              = 0
    , max_compact_bytes                    = 0
    , max_slop_bytes                       = 0
    , max_mem_in_use_bytes                 = 0
    , cumulative_live_bytes                = 0
    , copied_bytes                         = 0
    , par_copied_bytes                     = 0
    , cumulative_par_max_copied_bytes      = 0
# if MIN_VERSION_base(4,11,0)
    , cumulative_par_balanced_copied_bytes = 0
# if MIN_VERSION_base(4,12,0)
    , init_cpu_ns                          = 0
    , init_elapsed_ns                      = 0
# endif
# endif
    , mutator_cpu_ns                       = 0
    , mutator_elapsed_ns                   = 0
    , gc_cpu_ns                            = 0
    , gc_elapsed_ns                        = 0
    , cpu_ns                               = 0
    , elapsed_ns                           = 0
    , gc                                   = emptyGCDetails
    }

emptyGCDetails :: Stats.GCDetails
emptyGCDetails = Stats.GCDetails
    { gcdetails_gen                       = 0
    , gcdetails_threads                   = 0
    , gcdetails_allocated_bytes           = 0
    , gcdetails_live_bytes                = 0
    , gcdetails_large_objects_bytes       = 0
    , gcdetails_compact_bytes             = 0
    , gcdetails_slop_bytes                = 0
    , gcdetails_mem_in_use_bytes          = 0
    , gcdetails_copied_bytes              = 0
    , gcdetails_par_max_copied_bytes      = 0
# if MIN_VERSION_base(4,11,0)
    , gcdetails_par_balanced_copied_bytes = 0
# endif
    , gcdetails_sync_elapsed_ns           = 0
    , gcdetails_cpu_ns                    = 0
    , gcdetails_elapsed_ns                = 0
    }
#else
-- | Get GC statistics.
getGcStats :: IO Stats.GCStats
# if MIN_VERSION_base(4,6,0)
getGcStats = do
    enabled <- Stats.getGCStatsEnabled
    if enabled
        then Stats.getGCStats
        else return emptyGCStats

-- | Empty GC statistics, as if the application hasn't started yet.
emptyGCStats :: Stats.GCStats
emptyGCStats = Stats.GCStats
    { bytesAllocated         = 0
    , numGcs                 = 0
    , maxBytesUsed           = 0
    , numByteUsageSamples    = 0
    , cumulativeBytesUsed    = 0
    , bytesCopied            = 0
    , currentBytesUsed       = 0
    , currentBytesSlop       = 0
    , maxBytesSlop           = 0
    , peakMegabytesAllocated = 0
    , mutatorCpuSeconds      = 0
    , mutatorWallSeconds     = 0
    , gcCpuSeconds           = 0
    , gcWallSeconds          = 0
    , cpuSeconds             = 0
    , wallSeconds            = 0
    , parTotBytesCopied      = 0
    , parMaxBytesCopied      = 0
    }
# else
getGcStats = Stats.getGCStats
# endif

-- | Helper to work around rename in GHC.Stats in base-4.6.
gcParTotBytesCopied :: Stats.GCStats -> Int64
# if MIN_VERSION_base(4,6,0)
gcParTotBytesCopied = Stats.parTotBytesCopied
# else
gcParTotBytesCopied = Stats.parAvgBytesCopied
# endif
#endif

------------------------------------------------------------------------
-- * Sampling metrics

-- $sampling
-- The metrics register in the store can be sampled together. Sampling
-- is /not/ atomic. While each metric will be retrieved atomically,
-- the sample is not an atomic snapshot of the system as a whole. See
-- 'registerGroup' for an explanation of how to sample a subset of all
-- metrics atomically.

-- | A sample of some metrics.
type Sample = M.HashMap T.Text Value

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: Store -> IO Sample
sampleAll store = do
    state <- readIORef (storeState store)
    let metrics = stateMetrics state
        groups = stateGroups state
    cbSample <- sampleGroups $ IM.elems groups
    sample <- readAllRefs metrics
    let allSamples = sample ++ cbSample
    return $! M.fromList allSamples

-- | Sample all metric groups.
sampleGroups :: [GroupSampler] -> IO [(T.Text, Value)]
sampleGroups cbSamplers = concat `fmap` sequence (map runOne cbSamplers)
  where
    runOne :: GroupSampler -> IO [(T.Text, Value)]
    runOne GroupSampler{..} = do
        a <- groupSampleAction
        return $! map (\ (n, f) -> (n, f a)) (M.toList groupSamplerMetrics)

-- | The value of a sampled metric.
data Value = Counter {-# UNPACK #-} !Int64
           | Gauge {-# UNPACK #-} !Int64
           | Label {-# UNPACK #-} !T.Text
           | Distribution !Distribution.Stats
           deriving (Eq, Show)

sampleOne :: MetricSampler -> IO Value
sampleOne (CounterS m)      = Counter <$> m
sampleOne (GaugeS m)        = Gauge <$> m
sampleOne (LabelS m)        = Label <$> m
sampleOne (DistributionS m) = Distribution <$> m

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: M.HashMap T.Text (Either MetricSampler GroupId)
            -> IO [(T.Text, Value)]
readAllRefs m = do
    forM ([(name, ref) | (name, Left ref) <- M.toList m]) $ \ (name, ref) -> do
        val <- sampleOne ref
        return (name, val)

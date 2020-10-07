
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracing.Metrics
  ( KESMetricsData (..)
  , MaxKESEvolutions (..)
  , OperationalCertStartKESPeriod (..)
  , HasKESMetricsData (..)
  , ForgingStats (..)
  , ForgeThreadStats (..)
  , mapForgingCurrentThreadStats
  , mapForgingCurrentThreadStats_
  , mapForgingStatsTxsProcessed
  , mkForgingStats
  , threadStatsProjection
  ) where

import           Cardano.Prelude hiding (All, (:.:))

import           Cardano.Crypto.KES.Class (Period)
import           Control.Concurrent (myThreadId)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (All, hcmap, K (..), hcollapse)
import           Ouroboros.Consensus.Block (ForgeStateInfo, SlotNo)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.TypeFamilyWrappers (WrapForgeStateInfo (..))
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraForgeStateInfo (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node ()
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import           Shelley.Spec.Ledger.OCert (KESPeriod (..))


-- | KES-related data to be traced as metrics.
data KESMetricsData
  = NoKESMetricsData
  -- ^ The current protocol does not support KES.
  | TPraosKESMetricsData
      !Period
      -- ^ The current KES period of the hot key, relative to the start KES
      -- period of the operational certificate.
      !MaxKESEvolutions
      -- ^ The configured max KES evolutions.
      !OperationalCertStartKESPeriod
      -- ^ The start KES period of the configured operational certificate.

-- | The maximum number of evolutions that a KES key can undergo before it is
-- considered expired.
newtype MaxKESEvolutions = MaxKESEvolutions Word64

-- | The start KES period of the configured operational certificate.
newtype OperationalCertStartKESPeriod = OperationalCertStartKESPeriod Period

class HasKESMetricsData blk where
  -- Because 'ForgeStateInfo' is a type family, we need a Proxy argument to
  -- disambiguate.
  getKESMetricsData :: Proxy blk -> ForgeStateInfo blk -> KESMetricsData

  -- Default to 'NoKESMetricsData'
  getKESMetricsData _ _ = NoKESMetricsData

instance HasKESMetricsData (ShelleyBlock c) where
  getKESMetricsData _ forgeStateInfo =
      TPraosKESMetricsData currKesPeriod maxKesEvos oCertStartKesPeriod
    where
      HotKey.KESInfo
        { kesStartPeriod = KESPeriod startKesPeriod
        , kesEvolution = currKesPeriod
        , kesEndPeriod = KESPeriod endKesPeriod
        } = forgeStateInfo

      maxKesEvos = MaxKESEvolutions $
          fromIntegral $ endKesPeriod - startKesPeriod

      oCertStartKesPeriod = OperationalCertStartKESPeriod startKesPeriod

instance HasKESMetricsData ByronBlock where

instance All HasKESMetricsData xs => HasKESMetricsData (HardForkBlock xs) where
  getKESMetricsData _ forgeStateInfo =
        hcollapse
      . hcmap (Proxy @HasKESMetricsData) getOne
      . getOneEraForgeStateInfo
      $ forgeStateInfo
    where
      getOne :: forall blk. HasKESMetricsData blk
             => WrapForgeStateInfo blk
             -> K KESMetricsData blk
      getOne = K . getKESMetricsData (Proxy @blk) . unwrapForgeStateInfo

-- | This structure stores counters of blockchain-related events,
--   per individual forge thread.
--   These counters are driven by traces.
data ForgingStats
  = ForgingStats
  { fsTxsProcessedNum :: !(IORef Int)
    -- ^ Transactions removed from mempool.
  , fsState           :: !(IORef (Map ThreadId ForgeThreadStats))
  }

-- | Per-forging-thread statistics.
data ForgeThreadStats = ForgeThreadStats
  { ftsFirstSlot                 :: !SlotNo
    -- ^ First slot when the corresponding forging thread was active,
    --   which serves as point of origin for its statistics.
  , ftsLeadershipChecksInitiated :: !Int
  , ftsNodeCannotForgeNum        :: !Int
  , ftsNodeIsLeaderNum           :: !Int
  , ftsNodeNotLeaderNum          :: !Int
  , ftsBlocksForgedNum           :: !Int
  , ftsBlocksForgedInvalidNum    :: !Int
  , ftsSlotsMissedNum            :: !Int
    -- ^ Potentially missed slots.  Note that this is not the same as the number
    -- of missed blocks, since this includes all occurences of not reaching a
    -- leadership check decision, whether or not leadership was possible or not.
    --
    -- Also note that when the aggregate total for this metric is reported in the
    -- multi-pool case, it can be much larger than the actual number of slots
    -- occuring since node start, for it is a sum total for all threads.
  }

mkForgingStats :: IO ForgingStats
mkForgingStats =
  ForgingStats
    <$> newIORef 0
    <*> newIORef mempty

mapForgingStatsTxsProcessed ::
     ForgingStats
  -> (Int -> Int)
  -> IO Int
mapForgingStatsTxsProcessed fs f =
  atomicModifyIORef' (fsTxsProcessedNum fs) $
    \txCount -> (f txCount, txCount)

mapForgingCurrentThreadStats ::
     SlotNo
  -> ForgingStats
  -> (ForgeThreadStats -> (ForgeThreadStats, a))
  -> IO a
mapForgingCurrentThreadStats curSlot fs f = do
  tid <- myThreadId
  atomicModifyIORef' (fsState fs) $
    \tidStatsMap ->
      let initial = fromMaybe initialStats $ Map.lookup tid tidStatsMap
          (updated, ret) = f initial
      in (Map.alter (Just . const updated) tid tidStatsMap,
          ret)
 where
   initialStats = ForgeThreadStats curSlot 0 0 0 0 0 0 0

mapForgingCurrentThreadStats_ ::
     SlotNo
  -> ForgingStats
  -> (ForgeThreadStats -> ForgeThreadStats)
  -> IO ()
mapForgingCurrentThreadStats_ curSlot fs f = do
  tid <- myThreadId
  atomicModifyIORef' (fsState fs) $
    \tidStatsMap ->
      let initial = fromMaybe initialStats $ Map.lookup tid tidStatsMap
      in (Map.alter (Just . const (f initial)) tid tidStatsMap,
          ())
 where
   initialStats = ForgeThreadStats curSlot 0 0 0 0 0 0 0

threadStatsProjection ::
     ForgingStats
  -> (ForgeThreadStats -> a)
  -> IO [a]
threadStatsProjection fs f =
  fmap f . Map.elems <$> readIORef (fsState fs)

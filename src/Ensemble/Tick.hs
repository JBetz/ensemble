{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Tick where

import Data.Int
import Ensemble.Schema.TH

newtype Tick = Tick { tick_value :: Int }
    deriving newtype (Eq, Ord, Show, Enum, Num, Real, Integral)

steadyTimeToTick :: Double -> Int64 -> Tick
steadyTimeToTick sampleRate steadyTime = 
    Tick $ fromIntegral steadyTime * 1000 `div` floor sampleRate

tickToSteadyTime :: Double -> Tick -> Int64
tickToSteadyTime sampleRate (Tick tick) =
    (fromIntegral tick `div` 1000) * floor sampleRate

deriveJSONs
    [ ''Tick
    ]
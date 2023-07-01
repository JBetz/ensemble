{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Tick where

import Ensemble.Schema.TH

newtype Tick = Tick { tick_value :: Int }
    deriving newtype (Eq, Ord, Show, Enum, Num, Real, Integral)

deriveJSONs
    [ ''Tick
    ]
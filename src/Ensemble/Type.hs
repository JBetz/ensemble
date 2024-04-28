{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ensemble.Type where

import Control.Monad.Reader
import Ensemble.Server

newtype Ensemble a = Ensemble { unEnsemble :: ReaderT Server IO a }
    deriving newtype (Monad, Applicative, Functor, MonadReader Server, MonadIO)

{-# LANGUAGE DeriveAnyClass #-}

module Ensemble.Error where 

import Control.Exception

data ApiError = ApiError 
    { apiError_message :: String
    } deriving (Show, Eq)

deriving instance Exception ApiError

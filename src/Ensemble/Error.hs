module Ensemble.Error where 

data APIError = APIError 
    { apiError_message :: String
    , apiError_callstack :: Maybe String 
    } deriving (Show)

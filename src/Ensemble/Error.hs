module Ensemble.Error where 

data APIError = APIError 
    { apiError_message :: String
    } deriving (Show)

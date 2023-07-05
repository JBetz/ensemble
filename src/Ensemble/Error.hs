module Ensemble.Error where 

data ApiError = ApiError 
    { apiError_message :: String
    } deriving (Show, Eq)
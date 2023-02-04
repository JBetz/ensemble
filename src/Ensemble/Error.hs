module Ensemble.Error where 

newtype APIError = APIError { apiError_message :: String }
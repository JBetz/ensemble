module Main where

import Control.Monad
import Data.Aeson
import Data.String.Class (fromLazyByteString)
import Ensemble.Handler
import Ensemble.Server
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  server <- createServer
  forever $ do
    result <- receiveMessage server
    case result of
        Right outMessage -> do
            putStrLn $ fromLazyByteString (encode outMessage)
        Left errorMessage -> 
            hPutStrLn stderr errorMessage
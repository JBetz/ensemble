module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Ensemble.Handler
import Ensemble.Server
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  server <- createServer
  forever $ do
    maybeCommand <- getCommand
    case maybeCommand of
        Right command -> do
            result <- handle server command
            LBS.putStr $ encode result
        Left errorMessage -> 
            putStrLn $ "Invalid command: " <> errorMessage
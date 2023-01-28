module Main where

import Control.Monad
import Ensemble.Handler
import Ensemble.Server
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Starting ensemble ..."
  server <- createServer
  forever $ do
    maybeCommand <- getCommand
    case maybeCommand of
        Right command -> do
            putStr ">>\t"
            print command
            result <- handle server command
            putStr "<<\t"
            print result
        Left errorMessage -> 
            putStrLn $ "Invalid command: " <> errorMessage
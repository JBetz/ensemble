module Main where

import Control.Monad
import Ensemble

main :: IO ()
main = do
  putStrLn "Starting ensemble ..."
  sequencer <- createSequencer
  forever $ do
    maybeCommand <- getCommand
    case maybeCommand of
        Right command -> do
            putStr ">>\t"
            print command
            result <- handle sequencer command
            putStr "<<\t"
            print result
        Left errorMessage -> 
            putStrLn $ "Invalid command: " <> errorMessage
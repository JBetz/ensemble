{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Maybe
import Data.String.Class (fromLazyByteString)
import Ensemble.Config
import Ensemble.Handler
import Ensemble.Server
import Options.Generic
import System.IO
import Web.Scotty

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  server <- createServer
  config <- getRecord "Ensemble Audio Engine"
  case fromMaybe Interface_Http (interface config) of
    Interface_Pipe -> runPipeInterface server
    Interface_Http -> runHttpInterface server (fromMaybe 3000 $ port config)
  where                     
    runHttpInterface server port' = scotty port' $ do
        post "/send" $ do
          message <- jsonData
          result <- liftAndCatchIO $ receiveMessage server message
          json result
          
    runPipeInterface server = forever $ do
      eitherJsonMessage <- A.eitherDecodeStrict <$> BS.getLine
      case eitherJsonMessage of
        Right jsonMessage -> do
          result <- receiveMessage server jsonMessage
          putStrLn $ fromLazyByteString (A.encode result)
        Left parseError -> 
          hPutStrLn stderr $ "Parse error: " <> parseError

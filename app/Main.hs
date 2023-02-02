{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.String.Class (fromLazyByteString)
import Ensemble.API
import Ensemble.Handler
import Ensemble.Server
import System.IO
import Web.Scotty

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  server <- createServer
  _ <- forkIO $ scotty 3000 $ do

    post "/send" $ do
      message <- jsonData
      result <- liftAndCatchIO $ receiveMessage server message
      case result of 
        Right outMessage -> json outMessage
        Left errorMessage -> json (EnsembleError errorMessage)
          
  forever $ do
    eitherJsonMessage <- A.eitherDecodeStrict <$> BS.getLine
    case eitherJsonMessage of
      Right jsonMessage -> do
        result <- receiveMessage server jsonMessage
        case result of
            Right outMessage -> do 
                putStrLn $ fromLazyByteString (A.encode outMessage)
            Left errorMessage -> 
                hPutStrLn stderr errorMessage
      Left parseError -> 
        hPutStrLn stderr $ "Parse error: " <> parseError
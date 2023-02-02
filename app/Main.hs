{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.String.Class (fromLazyByteString)
import Ensemble.Handler
import Ensemble.Server
import System.IO
import Web.Scotty

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  server <- createServer
  runHttpInterface server
  where                     
    runHttpInterface server = scotty 3000 $ do
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

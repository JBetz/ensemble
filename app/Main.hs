{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Extra
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.IORef
import Data.Maybe
import Ensemble.Config
import Ensemble.Handler
import Ensemble.Server
import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Options.Generic
import System.IO
import Web.Scotty

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    config <- getRecord "Ensemble Audio Engine"
    server <- createServer config
    case interface config of
        Interface_Pipes -> runPipesInterface server
        Interface_Http -> runHttpInterface server (fromMaybe 3000 $ port config)
        Interface_WebSocket -> runWebSocketInterface server (fromMaybe 3000 $ port config)
        Interface_Library -> error "Can't use library as an executable"
  where              
    runPipesInterface server = do
        handleOutgoingMessages server
        forever $ BS.getLine >>= handleIncomingMessage server
    
    runHttpInterface server port' = do
        handleOutgoingMessages server
        scotty port' $ do
            post "/send" $ do
                incomingMessage <- jsonData
                outgoingMessage <- liftAndCatchIO $ receiveMessage server incomingMessage
                json outgoingMessage

    runWebSocketInterface server port' = do
        let warpSettings = Warp.setPort port' Warp.defaultSettings
        let websocketApp pendingConnection = do
                connection <- WS.acceptRequest pendingConnection
                sendThread <- forkIO $ forever $ do
                    outgoingMessage <- readChan $ server_messageChannel server
                    WS.sendTextData connection (A.encode outgoingMessage)
                isOpen <- newIORef True 
                whileM $ do
                    incomingMessage <- fmap Just (WS.receiveData connection) `catch` \case
                        WS.CloseRequest _ _ -> do
                            killThread sendThread
                            writeIORef isOpen False
                            pure Nothing
                        WS.ConnectionClosed -> do
                            killThread sendThread
                            writeIORef isOpen False
                            pure Nothing
                        WS.ParseException message -> do
                            putStrLn $ "PARSE EXCEPTION: " <> message
                            pure Nothing
                        WS.UnicodeException message -> do
                            putStrLn $ "UNICODE EXCEPTION: " <> message
                            pure Nothing
                    whenJust incomingMessage $ handleIncomingMessage server
                    readIORef isOpen
        let backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
        Warp.runSettings warpSettings $ WaiWs.websocketsOr WS.defaultConnectionOptions websocketApp backupApp

    handleIncomingMessage server message = 
        case A.eitherDecodeStrict message of
            Right incomingMessage -> do
                outgoingMessage <- receiveMessage server incomingMessage
                writeChan (server_messageChannel server) outgoingMessage
            Left parseError -> 
                hPutStrLn stderr $ "Parse error: " <> parseError

    handleOutgoingMessages server = 
        void $ forkIO $ forever $ do
            outgoingMessage <- readChan $ server_messageChannel server
            putStrLn $ LC8.unpack (A.encode outgoingMessage)

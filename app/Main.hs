{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Extra
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.IORef
import Data.Maybe
import Ensemble.Config
import Ensemble.Handler
import Ensemble.Env
import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Options.Generic
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    config <- getRecord "Ensemble Audio Engine"
    env <- createEnv config
    runWebSocketInterface env (fromMaybe 3000 $ port config)
  
  where              
    runWebSocketInterface env port' = do
        let warpSettings = Warp.setPort port' Warp.defaultSettings
        let websocketApp pendingConnection = do
                connection <- WS.acceptRequest pendingConnection
                sendThread <- forkIO $ forever $ do
                    outgoingMessage <- readChan $ env_messageChannel env
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
                    whenJust incomingMessage $ handleIncomingMessage env
                    readIORef isOpen
        let backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
        Warp.runSettings warpSettings $ WaiWs.websocketsOr WS.defaultConnectionOptions websocketApp backupApp

    handleIncomingMessage env message = 
        case A.eitherDecodeStrict message of
            Right incomingMessage -> do
                outgoingMessage <- receiveMessage env incomingMessage
                writeChan (env_messageChannel env) outgoingMessage
            Left parseError -> 
                hPutStrLn stderr $ "Parse error: " <> parseError

    handleOutgoingMessages env = 
        void $ forkIO $ forever $ do
            outgoingMessage <- readChan $ env_messageChannel env
            putStrLn $ LC8.unpack (A.encode outgoingMessage)

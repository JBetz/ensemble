{-# LANGUAGE ForeignFunctionInterface #-}

module Ensemble.Library where

import Control.Concurrent.Chan
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.IORef
import Ensemble.Config
import Ensemble.Handler
import Ensemble.Server
import Foreign.C.String
import System.IO
import System.IO.Unsafe

serverRef :: IORef (Maybe Server)
serverRef = unsafePerformIO $ newIORef Nothing

getServer :: IO Server
getServer = do
    maybeServer <- readIORef serverRef
    case maybeServer of
        Just server -> pure server
        Nothing -> do
            server <- createServer $ Config 
                { interface = Interface_Library
                , port = Nothing
                , logFile = Nothing
                }
            writeIORef serverRef $ Just server
            pure server

foreign export ccall ensemble_send :: CString -> IO ()
ensemble_send :: CString -> IO ()
ensemble_send cMessage = do
    message <- peekCString cMessage 
    server <- getServer
    case A.eitherDecodeStrict (C8.pack message) of
        Right incomingMessage -> do
            outgoingMessage <- receiveMessage server incomingMessage
            writeChan (server_messageChannel server) outgoingMessage
        Left parseError -> 
            hPutStrLn stderr $ "Parse error: " <> parseError

foreign export ccall ensemble_receive :: IO CString
ensemble_receive :: IO CString
ensemble_receive = do
    server <- getServer
    outgoingMessage <- readChan $ server_messageChannel server
    newCString $ LC8.unpack (A.encode outgoingMessage)

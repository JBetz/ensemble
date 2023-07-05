{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (show)

import Clap.Interface.Events
import Control.Monad.Freer
import Data.Either
import Ensemble.API
import Ensemble.Config
import Ensemble.Effects
import Ensemble.Event
import Ensemble.Server
import Ensemble.Schema.TH
import Ensemble.Tick
import Test.Hspec
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    server <- createServer defaultConfig
    result <- runEnsemble server $ do
        sendM $ putStrLn "startEngine"
        Ok <- startEngine

        sendM $ putStrLn "createPluginNode"
        nodeId <- createPluginNode (Argument "C:\\Program Files\\Common Files\\CLAP\\clap-saw-demo.clap") (Argument 0)
        
        sendM $ putStrLn "scheduleEvent - (1, NoteOn)"
        Ok <- scheduleEvent (Argument $ Tick 1) (Argument $ SequencerEvent
            { sequencerEvent_nodeId = nodeId
            , sequencerEvent_eventConfig = Nothing
            , sequencerEvent_event = Event_NoteOn $ NoteEvent
                { noteEvent_noteId = 0 
                , noteEvent_portIndex = 0
                , noteEvent_channel = 0
                , noteEvent_key = 60
                , noteEvent_velocity = 100
                }
            })

        sendM $ putStrLn "scheduleEvent - (1000, NoteOff)"
        Ok <- scheduleEvent (Argument $ Tick 1000) (Argument $ SequencerEvent
            { sequencerEvent_nodeId = nodeId
            , sequencerEvent_eventConfig = Nothing
            , sequencerEvent_event = Event_NoteOff $ NoteEvent
                { noteEvent_noteId = 0 
                , noteEvent_portIndex = 0
                , noteEvent_channel = 0
                , noteEvent_key = 60
                , noteEvent_velocity = 100
                }
            })

        sendM $ putStrLn "playSequence"
        Ok <- playSequence (Argument $ Tick 1) (Argument Nothing) (Argument False)

        sendM $ putStrLn "clearSequence"
        Ok <- clearSequence
        
        sendM $ putStrLn "stopEngine"
        Ok <- stopEngine
        pure ()
    print result
    hspec $ describe "result" $ 
        it "should not have an error" $ 
            result `shouldSatisfy` isRight


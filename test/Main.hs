{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (show)

import Clap.Interface.Events
import Clap.Interface.Extension.Params
import Clap.Interface.Id
import Control.Concurrent
import Control.Monad.Freer
import Data.Either
import Data.Traversable
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

        paths <- getPluginLocations
        _ <- scanForPlugins (Argument paths)
        
        sendM $ putStrLn "createPluginNode"
        nodeId <- createPluginNode (Argument "C:\\Program Files\\Common Files\\CLAP\\FluidSynth.clap\\FluidSynth.clap") (Argument 0)
        
        sendM $ putStrLn "getPluginParameters"
        parameters <- getPluginParameters (Argument nodeId)

        sendM $ putStrLn "getPluginParameterValue"
        _ <- for parameters $ \parameter ->
            getPluginParameterValue (Argument nodeId) (Argument $ clapId_id $ parameterInfo_id parameter)

        playNote nodeId 70 (Tick 1) 200
        playNote nodeId 72 (Tick 501) 200
        playNote nodeId 74 (Tick 1000) 200
        playNote nodeId 75 (Tick 1501) 200
        playNote nodeId 77 (Tick 2001) 200
        playNote nodeId 79 (Tick 2501) 200
        
        sendM $ putStrLn "playSequence"
        Ok <- playSequence (Argument $ Tick 1) (Argument Nothing) (Argument False)

        sendM $ threadDelay (5 * 1000 * 1000)

        sendM $ putStrLn "clearSequence"
        Ok <- clearSequence
        
        sendM $ putStrLn "stopEngine"
        Ok <- stopEngine
        pure ()

    hspec $ describe "result" $ 
        it "should not have an error" $ 
            result `shouldSatisfy` isRight

    where
        playNote nodeId key startTick duration = do
            Ok <- scheduleEvent (Argument startTick) (Argument $ SequencerEvent
                { sequencerEvent_nodeId = nodeId
                , sequencerEvent_eventConfig = Nothing
                , sequencerEvent_event = Event_NoteOn $ NoteEvent
                    { noteEvent_noteId = 0 
                    , noteEvent_portIndex = 0
                    , noteEvent_channel = 0
                    , noteEvent_key = key
                    , noteEvent_velocity = 120
                    }
                })

            Ok <- scheduleEvent (Argument $ startTick + duration) (Argument $ SequencerEvent
                { sequencerEvent_nodeId = nodeId
                , sequencerEvent_eventConfig = Nothing
                , sequencerEvent_event = Event_NoteOff $ NoteEvent
                    { noteEvent_noteId = 0 
                    , noteEvent_portIndex = 0
                    , noteEvent_channel = 0
                    , noteEvent_key = key
                    , noteEvent_velocity = 0
                    }
                })
            pure ()

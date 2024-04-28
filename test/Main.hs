{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (show)

import Clap.Interface.Events
import Clap.Interface.Extension.Params
import Clap.Interface.Id
import Control.Concurrent
import Control.Monad.Reader
import Data.Traversable
import Ensemble.API
import Ensemble.Config
import Ensemble.Effects
import Ensemble.Event
import Ensemble.Env
import Ensemble.Schema.TH
import Ensemble.Tick
import Test.Hspec
import System.Environment
import System.IO

main :: IO ()
main = do
    setEnv "FLUIDSYNTH_CLAP_DEBUG" "True"
    hSetBuffering stdout NoBuffering
    env <- createEnv defaultConfig
    result <- runEnsemble env $ do

        liftIO $ putStrLn "startEngine"
        Ok <- startEngine

        paths <- getPluginLocations
        _ <- scanForPlugins (Argument paths)
        
        liftIO $ putStrLn "createPluginNode"
        nodeId <- createPluginNode (Argument "/home/joe/ensemble/plugins/FluidSynth.clap") (Argument 0)
        
        liftIO $ putStrLn "getPluginParameters"
        parameters <- getPluginParameters (Argument nodeId)

        liftIO $ putStrLn "getPluginParameterValue"
        _ <- for parameters $ \parameter ->
            getPluginParameterValue (Argument nodeId) (Argument $ clapId_id $ parameterInfo_id parameter)

        playNote nodeId 70 (Tick 1) 200
        playNote nodeId 72 (Tick 501) 200
        playNote nodeId 74 (Tick 1000) 200
        playNote nodeId 75 (Tick 1501) 200
        playNote nodeId 77 (Tick 2001) 200
        playNote nodeId 79 (Tick 2501) 200
        
        liftIO $ putStrLn "playSequence"
        Ok <- playSequence (Argument $ Tick 1) (Argument Nothing) (Argument False)

        liftIO $ threadDelay (5 * 1000 * 1000)

        liftIO $ putStrLn "clearSequence"
        Ok <- clearSequence
        
        liftIO $ putStrLn "stopEngine"
        Ok <- stopEngine
        pure ()

    hspec $ describe "result" $ 
        it "should not have an error" $ 
            result `shouldSatisfy` (== ())

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
                    , noteEvent_velocity = 0.5
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
                    , noteEvent_velocity = 0.5
                    }
                })
            pure ()

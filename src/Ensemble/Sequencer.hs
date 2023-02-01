{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ensemble.Sequencer where

import Ensemble.Engine
import Ensemble.Event
import Data.Foldable (for_)
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Sound.PortAudio

data Sequencer = Sequencer
    { sequencer_currentTick :: IORef Tick
    , sequencer_scale :: IORef Double
    , sequencer_eventQueue :: IORef [(Tick, Event)]
    , sequencer_clients :: IORef (Map String EventCallback)
    }

newtype Tick = Tick { value :: Int }
    deriving (Eq, Ord, Show, Enum)

createSequencer :: IO Sequencer
createSequencer = do
    currentTick <- newIORef (Tick 0)
    scale <- newIORef 1000
    eventQueue <- newIORef mempty
    clients <- newIORef mempty
    pure $ Sequencer
        { sequencer_currentTick = currentTick
        , sequencer_scale = scale
        , sequencer_eventQueue = eventQueue
        , sequencer_clients = clients
        }

play :: Sequencer -> Engine -> Tick -> IO ()
play sequencer engine startTick = do
    writeIORef (sequencer_currentTick sequencer) startTick
    endTick <- getEndTick sequencer
    for_ [startTick .. endTick] $ process sequencer engine

getEndTick :: Sequencer -> IO Tick
getEndTick sequencer = do
    eventQueue <- readIORef $ sequencer_eventQueue sequencer
    pure $ maximum $ fst <$> eventQueue

sendAt :: Sequencer -> Tick -> Event -> IO ()
sendAt sequencer time event =
    modifyIORef' (sequencer_eventQueue sequencer) $ (<>) [(time, event)]

setTimeScale :: Sequencer -> Double -> IO ()
setTimeScale sequencer scale =
    writeIORef (sequencer_scale sequencer) scale

type EventCallback = Tick -> Event -> IO ()

registerClient :: Sequencer -> String -> EventCallback -> IO ()
registerClient sequencer name callback =
    modifyIORef' (sequencer_clients sequencer) $ Map.insert name callback

unregisterClient :: Sequencer -> String -> IO ()
unregisterClient sequencer name =
    modifyIORef' (sequencer_clients sequencer) $ Map.delete name

process :: Sequencer -> Engine -> Tick -> IO (Maybe Error)
process sequencer engine tick = do
    writeIORef (sequencer_currentTick sequencer) tick
    events <- readIORef (sequencer_eventQueue sequencer)
    let (activeEvents, remainingEvents) = partition (\(time, _) -> time <= tick) events 
    pushEvents engine $ snd <$> activeEvents
    outputs <- generateOutputs engine (fromIntegral $ engine_numberOfFrames engine)
    result <- sendOutputs engine (fromIntegral $ engine_numberOfFrames engine) outputs
    writeIORef (sequencer_eventQueue sequencer) remainingEvents
    pure result
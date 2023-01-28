module Ensemble.Sequencer where

import Ensemble.Engine
import Clap.Interface.Events
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Sound.PortAudio

data Sequencer = Sequencer
    { sequencer_currentTick :: IORef Tick
    , sequencer_scale :: IORef Double
    , sequencer_eventQueue :: IORef [(Tick, Destination, Event)]
    , sequencer_clients :: IORef (Map String EventCallback)
    }

newtype Tick = Tick { unTick :: Int }
    deriving (Eq, Ord, Show)

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

play :: Sequencer -> Tick -> IO ()
play sequencer startTick = do
    writeIORef (sequencer_currentTick sequencer) startTick

sendAt :: Sequencer -> Tick -> Destination -> Event -> IO ()
sendAt sequencer time destination event =
    modifyIORef' (sequencer_eventQueue sequencer) $ (<>) [(time, destination, event)]

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
    let (activeEvents, remainingEvents) = partition (\(time, _, _) -> time <= tick) events 
    pushEvents engine $ (\(_, destination, event) -> (destination, event)) <$> activeEvents
    outputs <- generateOutputs engine (fromIntegral $ engine_numberOfFrames engine)
    result <- sendOutputs engine (fromIntegral $ engine_numberOfFrames engine) outputs
    writeIORef (sequencer_eventQueue sequencer) remainingEvents
    pure result
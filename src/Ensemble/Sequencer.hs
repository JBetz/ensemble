module Ensemble.Sequencer where

import Ensemble.Engine
import Clap.Interface.Events
import Clap.Interface.Host
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

data Sequencer = Sequencer
    { sequencer_currentTick :: IORef Tick
    , sequencer_scale :: IORef Double
    , sequencer_eventQueue :: IORef [(Tick, Destination, Event)]
    , sequencer_clients :: IORef (Map String EventCallback)
    , sequencer_engine :: Engine
    }

newtype Tick = Tick { unTick :: Int }
    deriving (Eq, Ord, Show)

createSequencer :: IO Sequencer
createSequencer = do
    currentTick <- newIORef (Tick 0)
    scale <- newIORef 1000
    eventQueue <- newIORef mempty
    clients <- newIORef mempty
    engine <- createEngine defaultHostConfig
    _ <- start engine
    pure $ Sequencer
        { sequencer_currentTick = currentTick
        , sequencer_scale = scale
        , sequencer_eventQueue = eventQueue
        , sequencer_clients = clients
        , sequencer_engine = engine
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

process :: Sequencer -> Tick -> IO ()
process sequencer tick = do
    let engine = sequencer_engine sequencer
    writeIORef (sequencer_currentTick sequencer) tick
    events <- readIORef (sequencer_eventQueue sequencer)
    let (activeEvents, remainingEvents) = partition (\(time, _, _) -> time <= tick) events 
    pushEvents engine $ (\(_, destination, event) -> (destination, event)) <$> activeEvents
    generateOutputs engine (fromIntegral $ engine_numberOfFrames engine)
    -- sendOutputs engine (fromIntegral $ engine_numberOfFrames engine)
    writeIORef (sequencer_eventQueue sequencer) remainingEvents
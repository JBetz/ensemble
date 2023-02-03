{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Ensemble.Sequencer where

import Ensemble.Engine
import Ensemble.Event
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Sound.PortAudio as PortAudio

data Sequencer = Sequencer
    { sequencer_currentTick :: IORef Tick
    , sequencer_scale :: IORef Double
    , sequencer_eventQueue :: IORef [(Tick, SequencerEvent)]
    , sequencer_clients :: IORef (Map String EventCallback)
    }

newtype Tick = Tick { tick_value :: Int }
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

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

playSequence :: Sequencer -> Engine -> Tick -> IO ()
playSequence sequencer engine startTick = do
    writeIORef (sequencer_currentTick sequencer) startTick
    endTick <- getEndTick sequencer
    audioOutput <- render sequencer engine startTick endTick
    playAudio engine audioOutput

getEndTick :: Sequencer -> IO Tick
getEndTick sequencer = do
    eventQueue <- readIORef $ sequencer_eventQueue sequencer
    pure $ maximum $ fst <$> eventQueue

sendAt :: Sequencer -> Tick -> SequencerEvent -> IO ()
sendAt sequencer time event =
    modifyIORef' (sequencer_eventQueue sequencer) $ (<>) [(time, event)]

setTimeScale :: Sequencer -> Double -> IO ()
setTimeScale sequencer scale =
    writeIORef (sequencer_scale sequencer) scale

type EventCallback = Tick -> SequencerEvent -> IO ()

registerClient :: Sequencer -> String -> EventCallback -> IO ()
registerClient sequencer name callback =
    modifyIORef' (sequencer_clients sequencer) $ Map.insert name callback

unregisterClient :: Sequencer -> String -> IO ()
unregisterClient sequencer name =
    modifyIORef' (sequencer_clients sequencer) $ Map.delete name

render :: Sequencer -> Engine -> Tick -> Tick -> IO AudioOutput
render sequencer engine startTick endTick = do
    events <- getEventsBetween sequencer startTick endTick
    renderEvents 0 (groupEvents events)
    where 
        renderEvents :: Double -> [(Tick, [SequencerEvent])] -> IO AudioOutput
        renderEvents frameNumber = \case
            (Tick currentTick,events):(Tick nextTick,_):rest -> do
                pushEvents engine events
                let frameCount = fromIntegral (nextTick - currentTick) / 1000 * engine_sampleRate engine
                chunk <- generateOutputs engine (floor frameCount)
                remaining <- renderEvents (frameNumber + frameCount) rest
                pure $ chunk <> remaining
            (_lastTick,events):[] -> do
                pushEvents engine events
                -- One second of padding
                let frameCount = engine_sampleRate engine
                generateOutputs engine (floor frameCount)
            [] -> pure mempty

getEventsBetween :: Sequencer -> Tick -> Tick -> IO [(Tick, SequencerEvent)]
getEventsBetween sequencer startTick endTick = do
    events <- readIORef (sequencer_eventQueue sequencer)
    pure $ filter (\(tick, _) -> tick >= startTick && tick <= endTick) events

groupEvents :: [(Tick, SequencerEvent)] -> [(Tick, [SequencerEvent])]
groupEvents eventList =
    Map.toAscList $ Map.fromListWith (<>) $ (\(a, b) -> (a, [b])) <$> eventList

process :: Sequencer -> Engine -> Tick -> IO (Maybe PortAudio.Error)
process sequencer engine tick = do
    writeIORef (sequencer_currentTick sequencer) tick
    events <- readIORef (sequencer_eventQueue sequencer)
    let (activeEvents, remainingEvents) = partition (\(time, _) -> time <= tick) events 
    pushEvents engine $ snd <$> activeEvents
    outputs <- generateOutputs engine (fromIntegral $ engine_numberOfFrames engine)
    result <- sendOutputs engine (fromIntegral $ engine_numberOfFrames engine) outputs
    writeIORef (sequencer_eventQueue sequencer) remainingEvents
    pure result
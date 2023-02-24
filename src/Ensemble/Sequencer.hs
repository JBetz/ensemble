{-# LANGUAGE MonoLocalBinds #-}
module Ensemble.Sequencer where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Writer
import Data.Aeson
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Ensemble.Engine
import Ensemble.Error
import Ensemble.Event
import Ensemble.Schema.TaggedJSON

data Sequencer = Sequencer
    { sequencer_currentTick :: IORef Tick
    , sequencer_scale :: IORef Double
    , sequencer_eventQueue :: IORef [(Tick, SequencerEvent)]
    , sequencer_clients :: IORef (Map String EventCallback)
    }

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

playSequence :: (LastMember IO effs, Members '[Writer Value, Writer String, Error ApiError] effs) => Sequencer -> Engine -> Tick -> Eff effs ()
playSequence sequencer engine startTick = do
    sendM $ writeIORef (sequencer_currentTick sequencer) startTick
    endTick <- sendM $ getEndTick sequencer
    tellEvent PlaybackEvent_Rendering
    audioOutput <- render sequencer engine startTick endTick
    evaluatedAudioOutput <- sendM $ evaluate $ force audioOutput
    sendM $ stopInstruments engine
    tellEvent PlaybackEvent_Started
    playAudio engine evaluatedAudioOutput
    tellEvent PlaybackEvent_Stopped

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

render :: (Members '[Writer Value, Writer String, Error ApiError] effs, LastMember IO effs) => Sequencer -> Engine -> Tick -> Tick -> Eff effs AudioOutput
render sequencer engine startTick endTick = do
    events <- sendM $ getEventsBetween sequencer startTick endTick
    renderEvents 0 (groupEvents events)
    where 
        renderEvents frameNumber = \case
            (Tick currentTick,events):next@(Tick nextTick,_):rest -> do
                let frameCount = fromIntegral (nextTick - currentTick) / 1000 * engine_sampleRate engine
                chunk <- generateOutputs engine (floor frameCount) events
                remaining <- renderEvents (frameNumber + frameCount) (next:rest)
                pure $ chunk <> remaining
            (_lastTick,events):[] -> do
                -- One second of padding
                let frameCount = engine_sampleRate engine
                generateOutputs engine (floor frameCount) events
            [] -> pure $ AudioOutput [] []

getEventsBetween :: Sequencer -> Tick -> Tick -> IO [(Tick, SequencerEvent)]
getEventsBetween sequencer startTick endTick = do
    events <- readIORef (sequencer_eventQueue sequencer)
    pure $ filter (\(tick, _) -> tick >= startTick && tick <= endTick) events

groupEvents :: [(Tick, SequencerEvent)] -> [(Tick, [SequencerEvent])]
groupEvents eventList =
    Map.toAscList $ Map.fromListWith (<>) $ (\(a, b) -> (a, [b])) <$> eventList

tellEvent :: Member (Writer Value) effs => (HasTypeTag a, ToJSON a) => a -> Eff effs ()
tellEvent = tell . toTaggedJSON


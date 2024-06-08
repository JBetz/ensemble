{-# LANGUAGE MonoLocalBinds #-}

module Ensemble.Sequencer where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad.Reader
import Data.IORef
import Data.List (sortBy)
import qualified Data.Map as Map
import Ensemble.Engine
import Ensemble.Event
import Ensemble.Tick

data Sequencer = Sequencer
  { sequencer_eventQueue :: IORef [(Tick, SequencerEvent)]
  }

createSequencer :: IO Sequencer
createSequencer = do
  eventQueue <- newIORef mempty
  pure $ Sequencer {sequencer_eventQueue = eventQueue}

playSequenceOffline :: Sequencer -> Engine -> Tick -> Maybe Tick -> Bool -> IO ()
playSequenceOffline sequencer engine startTick maybeEndTick loop = do
  endTick <- case maybeEndTick of
    Just endTick -> pure endTick
    Nothing -> liftIO $ getEndTick sequencer
  audioOutput <- liftIO $ renderSequence sequencer engine startTick endTick
  evaluatedAudioOutput <- liftIO $ evaluate $ force audioOutput
  playAudio engine startTick loop evaluatedAudioOutput

playSequenceRealtime :: Sequencer -> Engine -> Tick -> Maybe Tick -> Bool -> IO ()
playSequenceRealtime sequencer engine startTick maybeEndTick loop = do
  endTick <- case maybeEndTick of
    Just endTick -> pure endTick
    Nothing -> liftIO $ getEndTick sequencer
  events <- liftIO $ getEventsBetween sequencer startTick endTick
  liftIO $ writeIORef (engine_steadyTime engine) 0
  let sortedEvents = sortBy (\(tickA, _) (tickB, _) -> compare tickA tickB) events
  runSequence endTick sortedEvents
  liftIO $ writeIORef (engine_steadyTime engine) (-1)
  where
    runSequence endTick [] = do
      currentTick <- liftIO $ getCurrentTick engine
      when (currentTick < endTick) $
        liftIO $
          threadDelay $
            (tick_value endTick - tick_value currentTick) * 1000
      when loop $
        playSequenceRealtime sequencer engine startTick maybeEndTick loop
    runSequence endTick events = do
      currentTick <- liftIO $ getCurrentTick engine
      let activeEvents = takeWhile (\(tick, _) -> tick <= currentTick) events
      modifyIORef' (engine_eventBuffer engine) (<> fmap snd activeEvents)
      threadDelay 10
      runSequence endTick $ drop (length activeEvents) events

getEndTick :: Sequencer -> IO Tick
getEndTick sequencer = do
  eventQueue <- readIORef $ sequencer_eventQueue sequencer
  pure $ maximum $ fst <$> eventQueue

sendAt :: Sequencer -> Tick -> SequencerEvent -> IO ()
sendAt sequencer time event =
  modifyIORef' (sequencer_eventQueue sequencer) $ (<>) [(time, event)]

type EventCallback = Tick -> SequencerEvent -> IO ()

renderSequence :: Sequencer -> Engine -> Tick -> Tick -> IO AudioOutput
renderSequence sequencer engine startTick endTick = do
  events <- getEventsBetween sequencer startTick endTick
  renderEvents $ groupEvents startTick endTick events
  where
    renderEvents = \case
      (Tick currentTick, events) : next@(Tick nextTick, _) : rest -> do
        let frameCount = floor $ fromIntegral (nextTick - currentTick) / 1000 * engine_sampleRate engine
        sendEvents engine events
        chunk <- receiveOutputs engine frameCount
        remaining <- renderEvents (next : rest)
        pure $ chunk <> remaining
      [(_lastTick, events)] -> do
        let frameCount = floor $ engine_sampleRate engine / 1000
        sendEvents engine events
        receiveOutputs engine frameCount
      [] -> pure $ AudioOutput [] []

getEvents :: Sequencer -> IO [SequencerEvent]
getEvents sequencer =
  fmap snd <$> readIORef (sequencer_eventQueue sequencer)

getEventsBetween :: Sequencer -> Tick -> Tick -> IO [(Tick, SequencerEvent)]
getEventsBetween sequencer startTick endTick = do
  events <- readIORef (sequencer_eventQueue sequencer)
  pure $ filter (\(tick, _) -> tick >= startTick && tick <= endTick) events

groupEvents :: Tick -> Tick -> [(Tick, SequencerEvent)] -> [(Tick, [SequencerEvent])]
groupEvents startTick endTick eventList =
  let tickIntervals = (\tick -> (tick, [])) <$> enumFromThenTo startTick (startTick + 100) (endTick + 100)
   in Map.toAscList $ Map.fromListWith (<>) $ tickIntervals <> ((\(a, b) -> (a, [b])) <$> eventList)

{-# LANGUAGE MonoLocalBinds #-}
module Ensemble.Sequencer where

import Control.Concurrent
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Writer
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.IORef
import Data.List (sortBy)
import qualified Data.Map as Map
import Ensemble.Engine
import Ensemble.Error
import Ensemble.Event
import Ensemble.Schema.TaggedJSON
import Ensemble.Tick
import GHC.Stack

data Sequencer = Sequencer
    { sequencer_eventQueue :: IORef [(Tick, SequencerEvent)]
    }

type SequencerEffects effs = (Members '[Writer (KeyMap Value), Writer String, Error ApiError] effs, LastMember IO effs, HasCallStack)

createSequencer :: IO Sequencer
createSequencer = do
    eventQueue <- newIORef mempty
    pure $ Sequencer { sequencer_eventQueue = eventQueue }

playSequence :: SequencerEffects effs => Sequencer -> Engine -> Tick -> Maybe Tick -> Bool -> Eff effs ()
playSequence sequencer engine startTick maybeEndTick _loop = do
    endTick <- case maybeEndTick of
        Just endTick -> pure endTick
        Nothing -> sendM $ getEndTick sequencer
    events <- sendM $ getEventsBetween sequencer startTick endTick
    sendM $ writeIORef (engine_steadyTime engine) 0
    tellEvent PlaybackEvent_Started
    runSequence $ sortBy (\(tickA, _) (tickB, _) -> compare tickA tickB) events
    tellEvent PlaybackEvent_Stopped
    sendM $ writeIORef (engine_steadyTime engine) (-1)
    where 
        runSequence [] = pure () 
        runSequence events = do 
            currentTick <- sendM $ getCurrentTick engine
            let activeEvents = takeWhile (\(tick, _) -> tick <= currentTick) events
            sendM $ modifyIORef' (engine_eventBuffer engine) (<> fmap snd activeEvents)
            tellEvent $ PlaybackEvent_CurrentTick currentTick
            sendM $ threadDelay 1000
            runSequence $ drop (length activeEvents) events

getEndTick :: Sequencer -> IO Tick
getEndTick sequencer = do
    eventQueue <- readIORef $ sequencer_eventQueue sequencer
    pure $ maximum $ fst <$> eventQueue

sendAt :: Sequencer -> Tick -> SequencerEvent -> IO ()
sendAt sequencer time event =
    modifyIORef' (sequencer_eventQueue sequencer) $ (<>) [(time, event)]

type EventCallback = Tick -> SequencerEvent -> IO ()

-- renderSequence :: SequencerEffects effs => Sequencer -> Engine -> Tick -> Tick -> Eff effs AudioOutput
-- renderSequence sequencer engine startTick endTick = do
--     events <- sendM $ getEventsBetween sequencer startTick endTick
--     renderEvents $ groupEvents startTick endTick events
--     where 
--         renderEvents = \case
--             (Tick currentTick,events):next@(Tick nextTick,_):rest -> do
--                 let frameCount = fromIntegral (nextTick - currentTick) / 1000 * engine_sampleRate engine
--                 chunk <- sendM $ generateOutputs engine (floor frameCount)
--                 remaining <- renderEvents (next:rest)
--                 pure $ chunk <> remaining
--             [(_lastTick,events)] -> do   
--                 let frameCount = floor (engine_sampleRate engine / 1000)
--                 sendM $ generateOutputs engine frameCount
--             [] -> pure $ AudioOutput [] []

getEvents :: Sequencer -> IO [SequencerEvent]
getEvents sequencer =
    fmap snd <$> readIORef (sequencer_eventQueue sequencer)
    
getEventsBetween :: Sequencer -> Tick -> Tick -> IO [(Tick, SequencerEvent)]
getEventsBetween sequencer startTick endTick = do
    events <- readIORef (sequencer_eventQueue sequencer)
    pure $ filter (\(tick, _) -> tick >= startTick && tick <= endTick) events

groupEvents :: Tick -> Tick -> [(Tick, SequencerEvent)] -> [(Tick, [SequencerEvent])]
groupEvents startTick endTick eventList =
    Map.toAscList $ Map.fromListWith (<>) $ (startTick,[]):(endTick,[]):((\(a, b) -> (a, [b])) <$> eventList)

tellEvent :: Member (Writer (KeyMap Value)) effs => (HasTypeTag a, ToJSON a) => a -> Eff effs ()
tellEvent = tell . toTaggedJSON


{-# LANGUAGE BangPatterns #-}

module Ensemble where

import Control.Monad
import Ensemble.Engine
import Ensemble.Sequencer
import Ensemble.Soundfont

main :: IO ()
main = do
  sequencer <- createSequencer
  let player = (engine_soundfontPlayer . sequencer_engine) sequencer
  void $ loadSoundfont player "soundfonts\\SGM-v2.01-NicePianosGuitarsBass-V1.2.sf2" True


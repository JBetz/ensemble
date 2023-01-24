{-# LANGUAGE BangPatterns #-}

module Ensemble where

import Clap.Interface.Host
import Control.Concurrent
import Control.Monad
import Ensemble.Engine
import Ensemble.Soundfont

main :: IO ()
main = do
  engine <- createEngine defaultHostConfig
  let player = engine_soundfontPlayer engine
  void $ loadSoundfont player "soundfonts\\SGM-v2.01-NicePianosGuitarsBass-V1.2.sf2" True
  noteOn player 0 80 100
  threadDelay $ 1000 * 1000
  noteOff player 0 80

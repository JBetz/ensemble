{-# LANGUAGE CPP #-}

module Ensemble.Window 
    ( createParentWindow 
    , showWindow
    , messagePump 
    ) where

#ifdef WINDOWS
import Ensemble.Window.Windows
#else
import Ensemble.Window.POSIX
#endif

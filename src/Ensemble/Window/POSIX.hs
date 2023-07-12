module Ensemble.Window.POSIX where

import Foreign.Ptr

showWindow :: Ptr () -> IO ()
showWindow = undefined

createParentWindow :: Maybe (Ptr ()) -> String -> Int -> Int -> IO (Ptr ())
createParentWindow _maybeParent _name _width _height = undefined

messagePump :: IO ()
messagePump = undefined

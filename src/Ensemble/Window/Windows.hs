{-# LANGUAGE ScopedTypeVariables #-}

module Ensemble.Window.Windows where

import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Foreign.Ptr
import qualified Graphics.Win32 as Win32
import System.Win32.DLL (getModuleHandle)

showWindow :: Win32.HWND -> IO ()
showWindow window = do 
    _ <- Win32.showWindow window Win32.sW_SHOWNORMAL
    Win32.updateWindow window

createParentWindow :: Maybe (Ptr ()) -> String -> Int -> Int -> IO (Ptr ())
createParentWindow _maybeParent name width height = do
    let className = Win32.mkClassName $ "Plugin GUI - " <> name
    icon         <- Win32.loadIcon   Nothing Win32.iDI_APPLICATION
    cursor       <- Win32.loadCursor Nothing Win32.iDC_ARROW
    bgBrush      <- Win32.createSolidBrush (Win32.rgb 0 0 255)
    mainInstance <- getModuleHandle Nothing
    _ <- Win32.registerClass
        ( Win32.cS_VREDRAW + Win32.cS_HREDRAW
        , mainInstance
        , Just icon
        , Just cursor
        , Just bgBrush
        , Nothing
        , className
        )
    let windowClosure = \_ _ _ _ -> pure 0
    Win32.createWindowEx
        Win32.wS_EX_TOPMOST     -- extended style 
        className               -- window class name
        name                    -- window name
        Win32.wS_OVERLAPPED     -- style
        Nothing                 -- left position
        Nothing                 -- top position 
        (Just width)            -- width
        (Just height)           -- height 
        Nothing                 -- parent
        Nothing                 -- menu
        mainInstance            -- application instance 
        windowClosure           -- WindowClosure (?)

messagePump :: IO ()
messagePump = Win32.allocaMessage $ \msg ->
    let pump = do
            r :: Either SomeException () <- try $ Win32.peekMessage msg Nothing 0 0 1
            when (either (const False) (const True)  r) $ do
                () <$ Win32.translateMessage msg
                () <$ Win32.dispatchMessage msg
                threadDelay $ 1000 * 100
                pump
    in pump

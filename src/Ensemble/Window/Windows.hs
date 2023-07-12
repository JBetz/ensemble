{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ensemble.Window.Windows where

import Control.Concurrent
import Control.Exception
import Foreign.Ptr
import qualified Graphics.Win32 as Win32
import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (LONG)

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
    let windowClosure = \window message wParam lParam -> Win32.defWindowProc (Just window) message wParam lParam
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
            result :: Either SomeException LONG <- try $ Win32.c_PeekMessage msg (Win32.maybePtr Nothing) 0 0 1
            case result of
                Right code -> if
                    | code == 0 -> threadDelay $ 1000 * 100
                    | code > 0 -> do
                        () <$ Win32.translateMessage msg
                        () <$ Win32.dispatchMessage msg
                    | otherwise -> 
                        print $ "Invalid peek message response: " <> show code 
                Left exception -> 
                    print exception
            pump    
    in pump

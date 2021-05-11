{- |
Module      : Disguise.Gtk.Main
Description : Various main routines for Gtk
Copyright   : Philip Kranz, 2018
License     : GPL-3
Maintainer  : pk@pmlk.net
Stability   : experimental
-}

module Disguise.Gtk.Main
  ( Controller (..)
  , run
  , syncIO
  , Processor (..)
  , keyProcessor

  , ioMain
  , pureMain
  , asyncMain
  , batchMain

  , defaultStyle
  , defaultStyleWithFont
  , quit
  ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.Trans
import Data.Functor.Contravariant
import Data.IORef
import Disguise.Gtk.Event
import Disguise.Cairo.Widget
import Disguise.Widget
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk as G

-- | See 'G.mainQuit'
quit :: IO ()
quit = G.mainQuit

defaultStyle :: IO Style
defaultStyle = defaultStyleWithFont "monospace 8"

defaultStyleWithFont :: String -> IO Style
defaultStyleWithFont f = do
  font <- G.fontDescriptionFromString f
  return Style
    { styleFont = font
    , styleColor0 = RGB 0 0 0
    , styleColor1 = RGB 1 1 1
    , styleColor2 = RGB 1 0 0
    }

newtype Controller ev = Controller
  { getController :: (CairoWidget (V Dim) (V Dim) (StyleT IO) -> IO ()) -> IO (ev -> IO ())
  }

instance Contravariant Controller where
  contramap f c = Controller $ \updater -> do
    handler <- getController c updater
    return $ \ev -> handler (f ev)

instance Semigroup (Controller a) where
  c1 <> c2 = Controller $ \updater -> do
    handler1 <- getController c1 updater
    handler2 <- getController c2 updater
    return $ \ev -> do
      handler1 ev
      handler2 ev

instance Monoid (Controller a) where
  mempty = Controller $ \updater -> return $ const $ return ()

newtype Processor ev = Processor
  { getProcessor :: G.Window -> (ev -> IO ()) -> IO ()
  }

instance Functor Processor where
  fmap f e = Processor $ \window handler -> getProcessor e window (handler . f)

instance Semigroup (Processor a) where
  p1 <> p2 = Processor $ \window handler -> do
    getProcessor p1 window handler
    getProcessor p2 window handler

instance Monoid (Processor a) where
  mempty = Processor $ \window handler -> return ()

keyProcessor :: Processor Event
keyProcessor = Processor $ \window handler -> do
  window `G.on` G.keyPressEvent $ do
    keyval <- G.eventKeyVal
    liftIO $ handler (KeyEvent keyval)
    return True
  return ()

run :: Style
    -> Processor e
    -> Controller e
    -> e
    -> IO ()
run style processor controller loadEvent = do
  G.initGUI
  widgetRef <- newIORef Nothing
  drawingArea <- G.drawingAreaNew
  window <- G.windowNew
  G.containerAdd window drawingArea
  drawingArea `G.on` G.draw $ do
    widget' <- liftIO $ readIORef widgetRef
    case widget' of
      Nothing -> return ()
      Just widget -> do
        G.Rectangle x y w h <- liftIO $ G.widgetGetAllocation drawingArea
        C.rectangle 0 0 (fromIntegral w) (fromIntegral h)
        setSourceRGB' (styleColor0 style)
        C.fill
        drawit <- liftIO $ drawFlowWidget widget (fromIntegral w) (fromIntegral h) style
        drawit
  window `G.on` G.deleteEvent $ do
    liftIO G.mainQuit
    return False
  handler <- getController controller $ \widget -> G.postGUIAsync $ do
    modifyIORef widgetRef (const (Just widget))
    G.widgetQueueDraw drawingArea
  G.widgetShowAll window
  getProcessor processor window handler
  handler loadEvent
  G.mainGUI

-- | A main function that takes an initial model, a function to transform the model when an event arrives
--   and a function from the model to a widget to display.
ioMain :: Style
       -> model
       -> (Event -> model -> IO model)
       -> (model -> IO (CairoWidget (V Dim) (V Dim) (StyleT IO)))
       -> IO ()
ioMain style initModel updateModel widget =
  run style keyProcessor (syncIO initModel updateModel widget) LoadEvent

-- | Same as 'ioMain' but without performing IO
pureMain :: Style
         -> model
         -> (Event -> model -> model)
         -> (model -> CairoWidget (V Dim) (V Dim) (StyleT IO))
         -> IO ()
pureMain style initModel updateModel widget =
  ioMain style initModel (fmap (fmap return) updateModel) (return . widget)

syncIO :: model
       -> (ev -> model -> IO model)
       -> (model -> IO (CairoWidget (V Dim) (V Dim) (StyleT IO)))
       -> Controller ev
syncIO initModel updateModel widget = Controller $ \updater -> do
  modelRef <- newIORef initModel
  sem <- newQSem 1
  return $ \ev -> do
    nextModel <- bracket_ (waitQSem sem) (signalQSem sem) $ do
      currentModel <- readIORef modelRef
      nextModel <- updateModel ev currentModel
      writeIORef modelRef nextModel
      return nextModel
    widget <- widget nextModel
    updater widget

-- | For use with typical FRP frameworks
asyncMain :: Style
          -> ((CairoWidget (V Dim) (V Dim) (StyleT IO) -> IO ()) -> IO (Event -> IO ()))
          -> IO ()
asyncMain style init = run style keyProcessor (Controller init) LoadEvent

batchMain :: Style -> ((CairoWidget (V Dim) (V Dim) (StyleT IO) -> IO ()) -> Chan Event -> IO ()) -> IO ()
batchMain style runner = do
  evchan <- newChan
  G.initGUI
  widgetRef <- newIORef Nothing
  drawingArea <- G.drawingAreaNew
  window <- G.windowNew
  G.containerAdd window drawingArea
  drawingArea `G.on` G.draw $ do
    widget' <- liftIO $ readIORef widgetRef
    case widget' of
      Nothing -> return ()
      Just widget -> do
        G.Rectangle x y w h <- liftIO $ G.widgetGetAllocation drawingArea
        C.rectangle 0 0 (fromIntegral w) (fromIntegral h)
        setSourceRGB' (styleColor0 style)
        C.fill
        drawit <- liftIO $ drawFlowWidget widget (fromIntegral w) (fromIntegral h) style
        drawit
  window `G.on` G.deleteEvent $ do
    liftIO G.mainQuit
    return False
  window `G.on` G.keyPressEvent $ do
    keyval <- G.eventKeyVal
    liftIO $ writeChan evchan (KeyEvent keyval)
    return True
  G.widgetShowAll window
  G.widgetQueueDraw drawingArea
  let draw = \widget -> G.postGUIAsync $ do
        modifyIORef widgetRef (const (Just widget))
        G.widgetQueueDraw drawingArea
  forkIO $ runner draw evchan >> G.mainQuit
  G.mainGUI

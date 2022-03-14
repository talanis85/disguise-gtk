{-# LANGUAGE GADTs #-}
module Disguise.Cairo.Widget.Text
  ( text
  , editText
  , textBox
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Disguise.Cairo.Widget hiding (clip, scale)
import Disguise.Widget
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

text :: (MonadIO f) => RGB -> FontDescription -> String -> CairoWidget (F Dim) (F Dim) f
text textcolor fontdesc str = FixedWidget $ do
  context <- liftIO $ cairoCreateContext Nothing
  layout <- liftIO $ layoutText context str
  liftIO $ layoutSetFontDescription layout (Just fontdesc)
  (_, PangoRectangle x y w h) <- liftIO $ layoutGetExtents layout
  let drawit = do
        setSourceRGB' textcolor
        showLayout layout
  return (max 0.1 w, h, drawit)

editText :: (MonadIO f) => RGB -> RGB -> FontDescription -> String -> Bool -> Int -> Widget (F Dim) (F Dim) f (Render (), Dim)
editText col1 col2 fontdesc str editing cursor = FixedWidget $ do
  let textcolor = if editing then col2 else col1
  context <- liftIO $ cairoCreateContext Nothing
  layout <- liftIO $ if cursor >= length str
                        then layoutText context (str ++ "_")
                        else layoutText context (insertCursorMarkup cursor str)
  liftIO $ layoutSetFontDescription layout (Just fontdesc)
  PangoRectangle cursorPos _ _ _ <- liftIO $ fst <$> layoutGetCursorPos layout cursor
  (_, PangoRectangle x y w h) <- liftIO $ layoutGetExtents layout
  let drawit = do
        setSourceRGB' textcolor
        showLayout layout
  return (w, h, (drawit, cursorPos))

textBox :: (MonadIO f) => RGB -> RGB -> FontDescription -> String -> Bool -> Int -> CairoWidget (V Dim) (V Dim) f
textBox col1 col2 fontdesc str editing cursor = FlowWidget $ \w h -> do
  case editText col1 col2 fontdesc str editing cursor of
    FixedWidget widget -> do
      (w', h', (r, cursorPos)) <- widget
      let drawit = do
            let s = h / h'
            retain $ do
              rectangle 0 0 w h
              clip
              scale s s
              if cursorPos > ((w / s) * 0.8)
                 then translate (- (cursorPos - ((w / s) * 0.8))) 0
                 else return ()
              r
      return drawit

insertCursorMarkup 0 (x:xs) = "<u>" ++ [x] ++ "</u>" ++ xs
insertCursorMarkup n (x:xs) = x : insertCursorMarkup (n-1) xs
insertCursorMarkup n [] = []

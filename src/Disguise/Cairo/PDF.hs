module Disguise.Cairo.PDF
  ( renderPdf
  ) where

import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango
import Disguise.Cairo

defaultStyle :: IO Style
defaultStyle = do
  font <- fontDescriptionFromString "monospace 8"
  return Style
    { styleFont = font
    , styleColor0 = RGB 1 1 1
    , styleColor1 = RGB 0 0 0
    , styleColor2 = RGB 1 0.5 0.5
    }

renderPdf :: FilePath -> Dim -> Dim -> [CairoWidget (V Dim) (V Dim) (StyleT IO)] -> IO ()
renderPdf fp w h widgets = do
  style <- defaultStyle
  withPDFSurface fp w h $ \surface -> do
    let renderWidget widget = do
          render <- drawFlowWidget widget w h style
          renderWith surface (render >> showPage)
    mapM_ renderWidget widgets

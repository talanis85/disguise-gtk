module Disguise.Cairo.PDF
  ( renderPdf
  ) where

import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango
import Disguise.Cairo

renderPdf :: FilePath -> Dim -> Dim -> [CairoWidget (V Dim) (V Dim) IO] -> IO ()
renderPdf fp w h widgets = do
  withPDFSurface fp w h $ \surface -> do
    let renderWidget widget = do
          render <- drawFlowWidget widget w h
          renderWith surface (render >> showPage)
    mapM_ renderWidget widgets

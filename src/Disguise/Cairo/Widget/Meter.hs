module Disguise.Cairo.Widget.Meter
  ( meterH
  ) where

import Control.Monad.Reader
import Disguise.Cairo.Widget hiding (fill)
import Disguise.Widget
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

meterH :: (Monad f) => Rational -> CairoWidget (V Dim) (V Dim) (StyleT f)
meterH cur = FlowWidget $ \w h -> do
  boxcol <- asks styleColor1
  innercol <- asks styleColor1
  let drawit = do
        setSourceRGB' innercol
        rectangle 0 0 (fromRational (toRational w * cur)) h
        fill
        setSourceRGB' boxcol
        rectangle 0 0 w h
        stroke
  return drawit

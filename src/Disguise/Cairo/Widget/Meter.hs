module Disguise.Cairo.Widget.Meter
  ( meterH
  ) where

import Control.Monad.Reader
import Disguise.Cairo.Widget hiding (fill)
import Disguise.Widget
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

meterH :: (Monad f) => RGB -> Rational -> CairoWidget (V Dim) (V Dim) f
meterH col cur = FlowWidget $ \w h -> do
  let drawit = do
        setSourceRGB' col
        rectangle 0 0 (fromRational (toRational w * cur)) h
        fill
        setSourceRGB' col
        rectangle 0 0 w h
        stroke
  return drawit

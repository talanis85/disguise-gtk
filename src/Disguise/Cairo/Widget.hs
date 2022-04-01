{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Disguise.Cairo.Widget
Description : Widgets to be displayed with Cairo
Copyright   : Philip Kranz, 2018
License     : BSD3
Maintainer  : pk@pmlk.net
Stability   : experimental
-}
module Disguise.Cairo.Widget
  ( 
  -- * Definition of Cairo widgets
    Dim
  , CairoWidget

  -- * Consumption
  , drawFlowWidget

  -- * Styling
  , loadFont
  , parseRGB
  , RGB (..)

  -- * Basic layout combinators
  , leftOf, topOf
  , tabularH, tabularV, tabularH', tabularV'
  , alignLeft, alignTop
  , space, spaceH, spaceV
  , stretchH, stretchV
  , scaleH, scaleV, scale
  , fill, box
  , pad
  , clipH, clipV, clip
  , zoom

  -- * Helpers
  , setSourceRGB'
  , retain

  -- * Re-exports
  , FontDescription
  ) where

import Control.Monad.Reader
import Data.Char (digitToInt)
import Data.Functor.Identity
import Data.Monoid
import Disguise.Widget
import Graphics.Rendering.Cairo hiding (clip, fill, scale)
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Pango

-- | Cairo uses 'Double' for coordinates
type Dim = Double

-- | Color type
data RGB = RGB Double Double Double

parseRGB :: String -> RGB
parseRGB ('#':r1:r2:g1:g2:b1:b2:[]) =
  RGB (hexcomp r1 r2) (hexcomp g1 g2) (hexcomp b1 b2)
    where
      hexcomp c1 c2 = fromIntegral (digitToInt c1 * 16 + digitToInt c2) / 255
parseRGB _ = error "Invalid RGB string"

reverseRGB :: RGB -> RGB
reverseRGB (RGB r g b) = RGB (1 - r) (1 - g) (1 - b)

loadFont :: String -> IO FontDescription
loadFont fontname = liftIO $ fontDescriptionFromString fontname

type CairoWidget w h f = Widget w h f (Render ())

retain r = save *> r <* restore

-- | Convert a 'FlowWidget' to a Cairo render action
drawFlowWidget :: (Functor f) => CairoWidget (V w) (V h) f -> w -> h -> f (Render ())
drawFlowWidget widget w h = case widget of
  FlowWidget widget' -> widget' w h

clip' :: Dim -> Dim -> Render () -> Render ()
clip' w h r = retain $ rectangle 0 0 w h >> Cairo.clip >> r

-- | Horizontal composition of widths
type family (:|) (w1 :: * -> *) (w2 :: * -> *) :: * -> * where
  F :| V = V
  F :| F = F
  V :| F = V

-- | Horizontal composition of heights
type family (:|-) (h1 :: * -> *) (h2 :: * -> *) :: * -> * where
  F :|- F = F
  F :|- V = F
  V :|- F = F
  V :|- V = V

-- | Arrange two widgets horizontally
leftOf :: (MonadFix f)
       => CairoWidget (w1 Dim) (h1 Dim) f
       -> CairoWidget (w2 Dim) (h2 Dim) f
       -> CairoWidget ((w1 :| w2) Dim) ((h1 :|- h2) Dim) f
leftOf (FixedWidget a) (FixedWidget b) = FixedWidget $ do
  (w1, h1, r1) <- a
  (w2, h2, r2) <- b
  return (w1 + w2, max h1 h2, retain r1 >> translate w1 0 >> retain r2)
leftOf (FixedWidget a) (FixedWidthWidget b) = FixedWidget $ do
  (w1, h1, r1) <- a
  (w2, r2) <- b h1
  return (w1 + w2, h1, retain r1 >> translate w1 0 >> retain r2)
leftOf (FixedWidget a) (FixedHeightWidget b) = FixedHeightWidget $ \w -> do
  (w1, h1, r1) <- a
  (h2, r2) <- b (max 0 (w - w1))
  return (max h1 h2, retain (clip' w1 (max h1 h2) r1) >> translate w1 0 >> retain r2)
leftOf (FixedWidget a) (FlowWidget b) = FixedHeightWidget $ \w -> do
  (w1, h1, r1) <- a
  r2 <- b (max 0 (w - w1)) h1
  return (h1, (retain (clip' w1 h1 r1) >> translate w1 0 >> retain r2))
leftOf (FixedWidthWidget a) (FixedWidget b) = FixedWidget $ do
  (w2, h2, r2) <- b
  (w1, r1) <- a h2
  return (w1 + w2, h2, retain r1 >> translate w1 0 >> retain r2)
leftOf (FixedWidthWidget a) (FixedWidthWidget b) = FixedWidthWidget $ \h -> do
  (w1, r1) <- a h
  (w2, r2) <- b h
  return (w1 + w2, retain r1 >> translate w1 0 >> retain r2)
leftOf (FixedWidthWidget a) (FixedHeightWidget b) = FixedHeightWidget $ \w -> mdo
  (h2, r2) <- b (max 0 (w - w1))
  (w1, r1) <- a h2
  return (h2, retain (clip' w1 h2 r1) >> translate w1 0 >> retain r2)
leftOf (FixedWidthWidget a) (FlowWidget b) = FlowWidget $ \w h -> do
  (w1, r1) <- a h
  r2 <- b (max 0 (w - w1)) h
  return (retain (clip' w1 h r1) >> translate w1 0 >> retain r2)
leftOf (FixedHeightWidget a) (FixedWidthWidget b) = FixedHeightWidget $ \w -> mdo
  (h1, r1) <- a (max 0 (w - w2))
  (w2, r2) <- b h1
  return (h1, retain r1 >> translate (max 0 (w - w2)) 0 >> retain (clip' w2 h1 r2))
leftOf (FlowWidget a) (FixedWidget b) = FixedHeightWidget $ \w -> do
  (w2, h2, r2) <- b
  r1 <- a (max 0 (w - w2)) h2
  return (h2, clip' w h2 (retain r1 >> translate (max 0 (w - w2)) 0 >> retain r2))
leftOf (FlowWidget a) (FixedWidthWidget b) = FlowWidget $ \w h -> do
  (w2, r2) <- b h
  r1 <- a (max 0 (w - w2)) h
  return (clip' w h (retain r1 >> translate (max 0 (w - w2)) 0 >> retain r2))

-- | Vertical composition of widths
type family (:-) (w1 :: * -> *) (w2 :: * -> *) :: * -> * where
  F :- F = F
  F :- V = F
  V :- F = F
  V :- V = V

-- | Vertical composition of heights
type family (:-|) (h1 :: * -> *) (h2 :: * -> *) :: * -> * where
  F :-| V = V
  F :-| F = F
  V :-| F = V

-- | Arrange two widgets vertically
topOf :: (MonadFix f)
      => CairoWidget (w1 Dim) (h1 Dim) f
      -> CairoWidget (w2 Dim) (h2 Dim) f
      -> CairoWidget ((w1 :- w2) Dim) ((h1 :-| h2) Dim) f
topOf (FixedWidget a) (FixedWidget b) = FixedWidget $ do
  (w1, h1, r1) <- a
  (w2, h2, r2) <- b
  return (max w1 w2, h1 + h2, retain r1 >> translate 0 h1 >> retain r2)
topOf (FixedWidget a) (FixedWidthWidget b) = FixedWidthWidget $ \h -> do
  (w1, h1, r1) <- a
  (w2, r2) <- b (max 0 (h - h1))
  return (max w1 w2, retain r1 >> translate 0 h1 >> retain (clip' w2 (max 0 (h - h1)) r2))
topOf (FixedWidget a) (FixedHeightWidget b) = FixedWidget $ do
  (w1, h1, r1) <- a
  (h2, r2) <- b w1
  return (w1, h1 + h2, retain r1 >> translate 0 h1 >> retain r2)
topOf (FixedWidget a) (FlowWidget b) = FixedWidthWidget $ \h -> do
  (w1, h1, r1) <- a
  r2 <- b w1 (max 0 (h - h1))
  return (w1, retain r1 >> translate 0 h1 >> retain (clip' w1 (max 0 (h - h1)) r2))
topOf (FixedWidthWidget a) (FixedWidget b) = FixedWidthWidget $ \h -> do
  (w2, h2, r2) <- b
  (w1, r1) <- a (max 0 (h - h2))
  return (max w1 w2, retain (clip' w1 (max 0 (h - h2)) r1) >> translate 0 (max 0 (h - h2)) >> retain r2)
topOf (FixedWidthWidget a) (FixedHeightWidget b) = FixedWidthWidget $ \h -> mdo
  (h2, r2) <- b w1
  (w1, r1) <- a (max 0 (h - h2))
  return (w1, retain (clip' w1 (max 0 (h - h2)) r1) >> translate 0 (max 0 (h - h2)) >> retain r2)
topOf (FixedHeightWidget a) (FixedWidget b) = FixedWidget $ do
  (w2, h2, r2) <- b
  (h1, r1) <- a w2
  return (w2, h1 + h2, retain r1 >> translate 0 h1 >> retain r2)
topOf (FixedHeightWidget a) (FixedWidthWidget b) = FixedWidthWidget $ \h -> mdo
  (h1, r1) <- a w2
  (w2, r2) <- b (max 0 (h - h1))
  return (w2, retain r1 >> translate 0 h1 >> retain (clip' w2 (max 0 (h - h1)) r2))
topOf (FixedHeightWidget a) (FixedHeightWidget b) = FixedHeightWidget $ \w -> do
  (h1, r1) <- a w
  (h2, r2) <- b w
  return (h1 + h2, retain r1 >> translate 0 h1 >> retain r2)
topOf (FixedHeightWidget a) (FlowWidget b) = FlowWidget $ \w h -> do
  (h1, r1) <- a w
  r2 <- b w (max 0 (h - h1))
  return (retain r1 >> translate 0 h1 >> retain (clip' w (max 0 (h - h1)) r2))
topOf (FlowWidget a) (FixedWidget b) = FixedWidthWidget $ \h -> do
  (w2, h2, r2) <- b
  r1 <- a w2 (max 0 (h - h2))
  return (w2, retain (clip' w2 (max 0 (h - h2)) r1) >> translate 0 (max 0 (h - h2)) >> retain r2)
topOf (FlowWidget a) (FixedHeightWidget b) = FlowWidget $ \w h -> do
  (h2, r2) <- b w
  r1 <- a w (max 0 (h - h2))
  return (retain (clip' w (max 0 (h - h2)) r1) >> translate 0 (max 0 (h - h2)) >> retain r2)

tabularH :: (MonadFix f)
         => [(Double, CairoWidget (V Dim) (h Dim) f)] -> CairoWidget (V Dim) (h Dim) f
tabularH [] = error "empty argument to 'tabularH'"
tabularH (x:xs) = case x of
  (_, FlowWidget _) -> tabularFlow (x:xs)
  (_, FixedHeightWidget _) -> tabularFixedHeight (x:xs)
  where
    relativeWidth w (a,b) = clipH (a * w) b
    tabularFlow ws = FlowWidget $ \w h -> do
      let widget' = foldr leftOf space (map (relativeWidth w) ws)
      runFlowWidget widget' w h
    tabularFixedHeight ws = FixedHeightWidget $ \w -> do
      let widget' = foldr leftOf spaceH (map (relativeWidth w) ws)
      runFixedHeightWidget widget' w

tabularH' :: (MonadFix f)
          => [CairoWidget (V Dim) (h Dim) f] -> CairoWidget (V Dim) (h Dim) f
tabularH' [] = error "empty argument to 'tabularH'"
tabularH' xs = tabularH $ map (\w -> (1.0 / fromIntegral (length xs), w)) xs

tabularV :: (MonadFix f)
         => [(Double, CairoWidget (w Dim) (V Dim) f)] -> CairoWidget (w Dim) (V Dim) f
tabularV [] = error "empty argument to 'tabularV'"
tabularV (x:xs) = case x of
  (_, FlowWidget _) -> tabularFlow (x:xs)
  (_, FixedWidthWidget _) -> tabularFixedWidth (x:xs)
  where
    relativeHeight h (a,b) = clipV (a * h) b
    tabularFlow ws = FlowWidget $ \w h -> do
      let widget' = foldr topOf space (map (relativeHeight h) ws)
      runFlowWidget widget' w h
    tabularFixedWidth ws = FixedWidthWidget $ \h -> do
      let widget' = foldr topOf spaceV (map (relativeHeight h) ws)
      runFixedWidthWidget widget' h

tabularV' :: (MonadFix f)
          => [CairoWidget (w Dim) (V Dim) f] -> CairoWidget (w Dim) (V Dim) f
tabularV' [] = error "empty argument to 'tabularV'"
tabularV' xs = tabularV $ map (\w -> (1.0 / fromIntegral (length xs), w)) xs

-- | Expand a widget horizontally by adding a space to the right
alignLeft :: (MonadFix f) => CairoWidget (F Dim) (h Dim) f -> CairoWidget (V Dim) ((h :|- V) Dim) f
alignLeft x = x `leftOf` space

-- | Expand a widget vertically by adding a space to the bottom
alignTop :: (MonadFix f) => CairoWidget (w Dim) (F Dim) f -> CairoWidget ((w :- V) Dim) (V Dim) f
alignTop x = x `topOf` space

-- | Expand a widget vertically by scaling along the Y axis
stretchV :: (Monad f) => CairoWidget w (F Dim) f -> CairoWidget w (V Dim) f
stretchV (FixedWidget widget) = FixedWidthWidget $ \h -> do
  (w', h', r) <- widget
  let drawit = do
        Cairo.scale 1.0 (h / h')
        retain r
  return (w', drawit)
stretchV (FixedHeightWidget widget) = FlowWidget $ \w h -> do
  (h', r) <- widget w
  let drawit = do
        Cairo.scale 1.0 (h / h')
        retain r
  return drawit

-- | Expand a widget horizontally by scaling along the X axis
stretchH :: (Monad f) => CairoWidget (F Dim) h f -> CairoWidget (V Dim) h f
stretchH (FixedWidget widget) = FixedHeightWidget $ \w -> do
  (w', h', r) <- widget
  let drawit = do
        Cairo.scale (w / w') 1.0
        retain r
  return (h', drawit)
stretchH (FixedWidthWidget widget) = FlowWidget $ \w h -> do
  (w', r) <- widget h
  let drawit = do
        Cairo.scale (w / w') 1.0
        retain r
  return drawit

-- | Scale a fixed widget horizontally, keeping the aspect ratio
scaleH :: (Monad f) => CairoWidget (F Dim) (F Dim) f -> CairoWidget (V Dim) (F Dim) f
scaleH (FixedWidget widget) = FixedHeightWidget $ \w -> do
  (w', h', r) <- widget
  let scaling = w / w'
  return (h' * scaling, Cairo.scale scaling scaling >> retain r)

-- | Scale a fixed widget vertically, keeping the aspect ratio
scaleV :: (Monad f) => CairoWidget (F Dim) (F Dim) f -> CairoWidget (F Dim) (V Dim) f
scaleV (FixedWidget widget) = FixedWidthWidget $ \h -> do
  (w', h', r) <- widget
  let scaling = h / h'
  return (w' * scaling, Cairo.scale scaling scaling >> retain r)

-- | Scale a fixed widget, keeping the aspect ratio
scale :: (Monad f) => CairoWidget (F Dim) (F Dim) f -> CairoWidget (V Dim) (V Dim) f
scale (FixedWidget widget) = FlowWidget $ \w h -> do
  (w', h', r) <- widget
  let scalew = w / w'
      scaleh = h / h'
      scaling = min scalew scaleh
  return (Cairo.scale scaling scaling >> retain r)

-- | Fill a widget's background
fill :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim) => RGB -> CairoWidget w h f -> CairoWidget w h f
fill col (FlowWidget widget) = FlowWidget $ \w h -> do
  r <- widget w h
  return (drawFill col w h r)
fill col (FixedWidget widget) = FixedWidget $ do
  (w, h, r) <- widget
  return (w, h, drawFill col w h r)
fill col (FixedWidthWidget widget) = FixedWidthWidget $ \h -> do
  (w, r) <- widget h
  return (w, drawFill col w h r)
fill col (FixedHeightWidget widget) = FixedHeightWidget $ \w -> do
  (h, r) <- widget w
  return (h, drawFill col w h r)

-- | Draw a box around a widget
box :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim) => RGB -> CairoWidget w h f -> CairoWidget w h f
box col (FlowWidget widget) = FlowWidget $ \w h -> do
  r <- widget w h
  return (drawBox col w h r)
box col (FixedWidget widget) = FixedWidget $ do
  (w, h, r) <- widget
  return (w, h, drawBox col w h r)
box col (FixedWidthWidget widget) = FixedWidthWidget $ \h -> do
  (w, r) <- widget h
  return (w, drawBox col w h r)
box col (FixedHeightWidget widget) = FixedHeightWidget $ \w -> do
  (h, r) <- widget w
  return (h, drawBox col w h r)

pad :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim) => Dim -> CairoWidget w h f -> CairoWidget w h f
pad px (FlowWidget widget) = FlowWidget $ \w h -> do
  r <- widget (w - 2 * px) (h - 2 * px)
  return (translate px px >> retain r)
pad px (FixedWidget widget) = FixedWidget $ do
  (w, h, r) <- widget
  return (w + 2 * px, h + 2 * px, translate px px >> retain r)
pad px (FixedWidthWidget widget) = FixedWidthWidget $ \h -> do
  (w, r) <- widget (h - 2 * px)
  return (w + 2 * px, translate px px >> retain r)
pad px (FixedHeightWidget widget) = FixedHeightWidget $ \w -> do
  (h, r) <- widget (w - 2 * px)
  return (h + 2 * px, translate px px >> retain r)

-- | Fix the width of a widget
clipH :: (Monad f, DimOf h ~ Dim) => Dim -> CairoWidget (V Dim) h f -> CairoWidget (F Dim) h f
clipH w (FlowWidget widget) = FixedWidthWidget $ \h -> do
  r <- widget w h
  return (w, retain (rectangle 0 0 w h >> Cairo.clip >> r))
clipH w (FixedHeightWidget widget) = FixedWidget $ do
  (h, r) <- widget w
  return (w, h, retain (rectangle 0 0 w h >> Cairo.clip >> r))

-- | Fix the height of a widget
clipV :: (Monad f, DimOf w ~ Dim) => Dim -> CairoWidget w (V Dim) f -> CairoWidget w (F Dim) f
clipV h (FlowWidget widget) = FixedHeightWidget $ \w -> do
  r <- widget w h
  return (h, retain (rectangle 0 0 w h >> Cairo.clip >> r))
clipV h (FixedWidthWidget widget) = FixedWidget $ do
  (w, r) <- widget h
  return (w, h, retain (rectangle 0 0 w h >> Cairo.clip >> r))

-- | Fix width and height
clip :: (Monad f) => Dim -> Dim -> CairoWidget (V Dim) (V Dim) f -> CairoWidget (F Dim) (F Dim) f
clip w h = clipV h . clipH w

drawBox col w h r = do
  setSourceRGB' col
  rectangle 0 0 w h
  stroke
  retain r

drawFill col w h r = do
  setSourceRGB' col
  rectangle 0 0 w h
  Cairo.fill
  retain r

-- | Zoom and pan
zoom :: (Monad f) => Double -> Dim -> Dim -> CairoWidget (V Dim) (V Dim) f -> CairoWidget (V Dim) (V Dim) f
zoom scale panx pany (FlowWidget widget) = FlowWidget $ \w h -> do
  r <- widget (w * scale) (h * scale)
  return (clip' w h (retain (translate panx pany >> r)))

-- | A space of variable size
space :: (Applicative f) => CairoWidget (V w) (V h) f
space = FlowWidget $ \w h -> pure (return ())

spaceH :: (Applicative f, Num h) => CairoWidget (V w) (F h) f
spaceH = FixedHeightWidget $ \w -> pure (0, return ())

spaceV :: (Applicative f, Num w) => CairoWidget (F w) (V h) f
spaceV = FixedWidthWidget $ \h -> pure (0, return ())

setSourceRGB' (RGB a b c) = setSourceRGB a b c

{-# LANGUAGE TypeOperators #-}

module Disguise.Cairo.Widget.List
  ( list
  , listOf
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Zipper
import Disguise.Cairo.Widget hiding (clip)
import Disguise.Widget
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

iteratePlus :: (MonadPlus m) => (a -> m a) -> a -> m a
iteratePlus f x = f x `mplus` join (iteratePlus f <$> f x)

matchZipper :: Maybe (h :>> a) -> ([a], Maybe a, [a])
matchZipper Nothing = ([], Nothing, [])
matchZipper (Just z) = (reverse (iterateFocus leftward z), Just (z ^. focus), iterateFocus rightward z)
  where
    iterateFocus f z = map (view focus) (iteratePlus f z)

sliceZipper :: Maybe (h :>> a) -> Int -> ([a], Maybe a, [a])
sliceZipper z n = let (l, c, r) = matchZipper z
                      l' = drop (max 0 (length l - (n `div` 2))) l
                      r' = case c of
                             Nothing -> take (n - length l') r
                             Just _  -> take (n - length l' - 1) r
                  in (l', c, r')

list :: (MonadIO f)
     => RGB
     -> RGB
     -> FontDescription
     -> Maybe (h :>> a)
     -> (a -> String)
     -> CairoWidget (V Dim) (V Dim) f
list col selcol fontdesc zipper display = FlowWidget $ \w h -> do
  let textcolor = col
  let seltextcolor = selcol
  context <- liftIO $ cairoCreateContext Nothing
  layout0 <- liftIO $ layoutText context "J"
  liftIO $ layoutSetFontDescription layout0 (Just fontdesc)
  (_, PangoRectangle _ _ _ lineh) <- liftIO $ layoutGetExtents layout0
  let drawit = do
        let itemcount = floor (h / lineh)
            (l, c, r) = sliceZipper zipper itemcount
            drawLine selected item = do
              layout <- liftIO $ layoutText context (display item)
              liftIO $ layoutSetFontDescription layout (Just fontdesc)
              rectangle 0 0 w lineh
              clip
              setSourceRGB' $ if selected then seltextcolor else textcolor
              showLayout layout
              resetClip
              translate 0 lineh
        mapM_ (drawLine False) l
        maybe (return ()) (drawLine True) c
        mapM_ (drawLine False) r
  return drawit

listOf :: (MonadIO f, MonadFix f)
       => Maybe (h :>> a) -> (a -> Bool -> CairoWidget (V Dim) (F Dim) f) -> CairoWidget (V Dim) (V Dim) f
listOf zipper display = FlowWidget $ \w h -> do
  let (l, c, r) = matchZipper zipper
      lw = foldr topOf (fixh 0 space) (map ($ False) (map display l))
      rw = foldr topOf (fixh 0 space) (map ($ False) (map display r))
  (lheight, ldraw) <- runFixedHeightWidget lw w
  (cheight, cdraw) <- case c of
    Nothing -> return (0, return ())
    Just c' -> runFixedHeightWidget (display c' True) w
  (rheight, rdraw) <- runFixedHeightWidget rw w
  let translation | lheight + cheight + rheight <= h = 0
                  | lheight < h / 2 = 0
                  | otherwise = h / 2 - lheight
  return $ do
    rectangle 0 0 w h
    clip
    translate 0 translation
    retain ldraw
    translate 0 lheight
    retain cdraw
    translate 0 cheight
    retain rdraw

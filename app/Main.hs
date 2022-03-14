module Main where

import Control.Lens
import Control.Zipper

import Disguise.Gtk.Main
import Disguise.Cairo
import Disguise.Cairo.Widget.List
import Disguise.Cairo.PDF

main :: IO ()
main = do
  font <- loadFont "monospace 8"

  putStrLn "Generating test.pdf"
  renderPdf "test.pdf" 100 20 [ui font]

  putStrLn "Displaying GTK UI"
  pureMain () (const id) (const (ui font))

ui :: FontDescription -> CairoWidget (V Dim) (V Dim) IO
ui font =
  (box (RGB 0.5 0.5 0.5) (stretchH (text (RGB 0.5 0.5 0.5) font "FOO")) `leftOf` box (RGB 0.5 0.5 0.5) (stretchV (text (RGB 0.5 0.5 0.5) font "BAR")))
  `topOf`
  testList font

testList :: FontDescription -> CairoWidget (V Dim) (V Dim) IO
testList font = list (RGB 0.5 0.5 0.5) (RGB 0.5 0 0) font
  (within traversed (zipper ["hello", "world", "this", "is", "a", "list"])) id

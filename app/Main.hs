module Main where

import Control.Lens
import Control.Zipper

import Disguise.Gtk.Main
import Disguise.Cairo
import Disguise.Cairo.Widget.List
import Disguise.Cairo.PDF

main :: IO ()
main = do
  putStrLn "Generating test.pdf"
  renderPdf "test.pdf" 100 20 [ui]

  putStrLn "Displaying GTK UI"
  style <- defaultStyle
  pureMain style () (const id) (const ui)

ui :: CairoWidget (V Dim) (V Dim) (StyleT IO)
ui =
  (box (stretchH (text "FOO")) `leftOf` box (stretchV (text "BAR")))
  `topOf`
  testList

testList :: CairoWidget (V Dim) (V Dim) (StyleT IO)
testList = list (within traversed (zipper ["hello", "world", "this", "is", "a", "list"]))

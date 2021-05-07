{- |
Module      : Disguise.Gtk.Event
Description : Events
Copyright   : Philip Kranz, 2018
License     : GPL-3
Maintainer  : pk@pmlk.net
Stability   : experimental
-}
module Disguise.Gtk.Event
  ( Event (..)
  , keyName
  , keyChar
  ) where

import qualified Graphics.UI.Gtk as Gtk
import qualified Data.Text as T

data Event
  = LoadEvent
  | KeyEvent Gtk.KeyVal

keyName :: Gtk.KeyVal -> String
keyName = T.unpack . Gtk.keyName

keyChar :: Gtk.KeyVal -> Maybe Char
keyChar = Gtk.keyToChar
